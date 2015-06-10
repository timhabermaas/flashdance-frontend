module Flashdance where

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Signal exposing (Address, Signal)
import List as L
import Date
import Date.Format as DF
import Color exposing (..)
import String
import Model as M
import Task.Extra as TE
import Http
import Task exposing (Task, andThen)
import HttpRequests exposing (..)
import Price

type alias GigId = String
type alias SeatId = String
type alias OrderId = String
type alias Name = String
type alias Email = String

type Action
  = NoOp
  | ClickSeat M.Seat
  | SeatsReceived (List M.Seat, List M.Row)
  | GigsReceived (List Gig)
  | ClickGig Gig
  | ReservationsReceived (List M.Reservation)
  | OrderTicket Gig Name Email (List M.Seat) String
  | UpdateEmail String
  | UpdateName String
  | UpdateReducedCount String
  | UpdateStreet String
  | UpdatePostalCode String
  | UpdateCity String
  | HttpOrderFailed String
  | CloseFlashMessage
  | StartOrder Name Email
  | OrderStarted String
  | SeatReserved SeatId
  | SeatSelected SeatId
  | SeatUnselected SeatId
  | ShowErrorFlashMessage String
  | UpdateDeliveryOption DeliveryOption
  | FinishOrderTicket OrderId String -- TODO this should be an integer

type Effect
  = FetchSeats GigId
  | SubmitOrder GigId Name Email (List M.Seat) Int
  | StartOrderRequest Name Email
  | ReserveSeatRequest OrderId SeatId
  | FreeSeatRequest OrderId SeatId
  | FinishOrderRequest OrderId OrderInfo

type CurrentPage = GigIndex | GigView Gig -- | TransitionTo CurrentPage
type FlashMessage = Info String | Error String | Hidden
type DeliveryOption = PickUpBoxOffice | PickUpBeforehand | Delivery Address'
type alias OrderInfo = { name: String, email: String, id: OrderId, deliveryOption: Maybe DeliveryOption }
type OrderState = Ordering OrderInfo | Browsing
type alias Address' = { street: String, postalCode: String, city: String }
type alias CurrentFormInput = { name: String, email: String, reduced: String }
type alias Gig = { id: GigId, date: Date.Date, title: String, freeSeats: Int }
type alias Model = { formInput: CurrentFormInput, stand: M.Model, gigs: List Gig, orderState: OrderState, page: CurrentPage, flashMessage: FlashMessage, innerFlashMessage: FlashMessage }

emptyAddress : Address'
emptyAddress = { street = "", postalCode = "", city = "" }

initialModel : Model
initialModel = { page = GigIndex, gigs = [], stand = M.initialModel, formInput = { name = "", email = "", reduced = "0"}, flashMessage = Hidden, orderState = Browsing, innerFlashMessage = Hidden }

updateAddress : Model -> Address' -> Model
updateAddress model address =
  case model.orderState of
    Ordering order ->
      case order.deliveryOption of
        Just (Delivery a) -> {model | orderState <- Ordering ({order | deliveryOption <- Just (Delivery address)})}
        _ -> model
    _ -> model

forcefullyExtractAddress : Model -> Address'
forcefullyExtractAddress model =
  case model.orderState of
    Ordering order ->
      case order.deliveryOption of
        Just (Delivery a) -> a

updateCity : Address' -> String -> Address'
updateCity a c = {a | city <- c}

updateStreet : Address' -> String -> Address'
updateStreet a s = {a | street <- s}

updatePostalCode : Address' -> String -> Address'
updatePostalCode a p = {a | postalCode <- p}

update : Action -> (Model, Maybe Effect) -> (Model, Maybe Effect)
update action (model, _) =
  case action of
    ClickSeat seat -> case model.orderState of
      Ordering order ->
        if M.isSelected model.stand seat then
          (model, Just (FreeSeatRequest order.id seat.id))
        else
          (model, Just (ReserveSeatRequest order.id seat.id))
      _ -> (model, Nothing)
    SeatReserved seatId -> ({model | stand <- M.reserveSeats model.stand [unwrapMaybe <| M.findSeat model.stand seatId]}, Nothing)
    SeatSelected seatId -> ({model | stand <- M.selectSeat model.stand <| unwrapMaybe <| M.findSeat model.stand seatId}, Nothing)
    SeatUnselected seatId -> ({model | stand <- M.unselectSeat model.stand <| unwrapMaybe <| M.findSeat model.stand seatId}, Nothing)
    SeatsReceived (seats, rows) -> ({model | stand <- M.updateSeats model.stand seats rows}, Nothing)
    GigsReceived gigs -> ({model | gigs <- gigs}, Nothing)
    ReservationsReceived r -> ({model | stand <- M.updateReservations model.stand r}, Nothing)
    ClickGig gig -> ({model | page <- GigView gig}, Just <| FetchSeats gig.id)
    OrderTicket gig name email seats reduced ->
      case String.toInt reduced of
        Result.Ok x -> ({model | stand <- M.clearSelections <| M.reserveSeats model.stand seats}, Just <| SubmitOrder gig.id name email seats x)
        Result.Err x -> (model, Nothing)
    UpdateEmail email -> ({model | formInput <- {name = model.formInput.name, email = email, reduced = model.formInput.reduced}}, Nothing)
    UpdateName name -> ({model | formInput <- {name = name, email = model.formInput.email, reduced = model.formInput.reduced}}, Nothing)
    UpdateDeliveryOption option ->
      case model.orderState of
        Browsing -> (model, Nothing)
        Ordering order -> ({model | orderState <- Ordering ({name = order.name, email = order.email, id = order.id, deliveryOption = Just option})}, Nothing)
    UpdateReducedCount reduced ->
      if reducedCountValid reduced then
        -- TODO needs more sophistacted approach, we reset the error field even if there might still be errors
        ({model | innerFlashMessage <- Hidden, formInput <- {name = model.formInput.name, email = model.formInput.email, reduced = reduced}}, Nothing)
      else
        ({model | innerFlashMessage <- Error "Bitte gültige Anzahl an ermäßigten Karten angeben.", formInput <- {name = model.formInput.name, email = model.formInput.email, reduced = reduced}}, Nothing)
    UpdateCity city -> (updateAddress model (updateCity (forcefullyExtractAddress model) city), Nothing)
    UpdateStreet street -> (updateAddress model (updateStreet (forcefullyExtractAddress model) street), Nothing)
    UpdatePostalCode pc -> (updateAddress model (updatePostalCode (forcefullyExtractAddress model) pc), Nothing)
    HttpOrderFailed error -> ({model | flashMessage <- (Error error)}, Nothing)
    CloseFlashMessage -> ({model | flashMessage <- Hidden}, Nothing)
    StartOrder name email -> (model, Just <| StartOrderRequest name email) -- TODO send request to server
    OrderStarted orderId -> ({model | orderState <- Ordering {name = model.formInput.name, email = model.formInput.email, id = orderId, deliveryOption = Nothing}}, Nothing)
    ShowErrorFlashMessage message -> ({model | flashMessage <- Error message}, Nothing)
    NoOp -> (model, Nothing)




port fetchGigs : Task Http.Error ()
port fetchGigs =
  HttpRequests.fetchGigs `Task.andThen` (\gigs -> Signal.send actions.address (GigsReceived gigs))


seatsRequestGigIdSignal : Signal (Maybe GigId)
seatsRequestGigIdSignal =
  let toId effect =
    case effect of
      Just (FetchSeats id) -> Just id
      _ -> Nothing
  in
    Signal.map toId effects


port seatsRequest : Signal (Task Http.Error (List Task.ThreadID))
port seatsRequest =
  let send gigId = case gigId of
    Just id ->
      TE.parallel
        [(HttpRequests.fetchSeats id) `Task.andThen` (\r -> Signal.send actions.address (SeatsReceived r)),
         (HttpRequests.fetchReservations id) `Task.andThen` (\r -> Signal.send actions.address (ReservationsReceived r))
        ]
    Nothing -> Task.sequence [Task.spawn (Task.succeed [()])]

  in Signal.map send seatsRequestGigIdSignal


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
  case error of
    Http.Timeout -> "Server nicht erreichbar."
    Http.NetworkError -> "Server nicht erreichbar."
    Http.UnexpectedPayload response -> "Error"
    Http.BadResponse status response -> response

port orderRequest : Signal (Task Http.Error ())
port orderRequest =
  let maybeRequest s = case s of
    -- TODO the `andThen` is only here to satisfy the type checker
    --      we probably want to redirect the user anyway.
    Just (SubmitOrder id name email seats reducedCount) ->
      (HttpRequests.submitOrder id name email (List.map .id seats) reducedCount
      `Task.andThen` (\result -> Signal.send actions.address NoOp))
      `Task.onError` (\error -> Signal.send actions.address (HttpOrderFailed (httpErrorToMessage error)))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects


port orderStartRequest : Signal (Task Http.Error ())
port orderStartRequest =
  let maybeRequest s = case s of
    Just (StartOrderRequest name email) ->
      (HttpRequests.startOrder name email)
      `Task.andThen` (\orderId -> Signal.send actions.address (OrderStarted orderId))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects

port reserveSeatRequest : Signal (Task Http.Error ())
port reserveSeatRequest =
  let maybeRequest s = case s of
    Just (ReserveSeatRequest orderId seatId) ->
      (HttpRequests.reserveSeat orderId seatId)
      `Task.andThen` (\result -> Signal.send actions.address (SeatSelected seatId))
      `Task.onError` (\result -> (Signal.send actions.address (ShowErrorFlashMessage "Sitz ist schon reserviert."))
        `Task.andThen` (\foo -> Signal.send actions.address (SeatReserved seatId)))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects

port freeSeatRequest : Signal (Task Http.Error ())
port freeSeatRequest =
  let maybeRequest s = case s of
    Just (FreeSeatRequest orderId seatId) ->
      (HttpRequests.freeSeat orderId seatId)
      `Task.andThen` (\result -> Signal.send actions.address (SeatUnselected seatId))
      `Task.onError` (\result -> (Signal.send actions.address (ShowErrorFlashMessage "Sitz konnte nicht entfernt werden.")))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects


drawGigEntry : Address Action -> Gig -> H.Html
drawGigEntry address gig =
  H.li []
    [ H.a [HA.href "#", HE.onClick address (ClickGig gig)]
      [ H.text gig.title
      , H.span [HA.class "badge"]
        [ H.text <| (toString gig.freeSeats) ++ " freie Plätze" ]
      ]
    ]

viewFlashMessage : Address Action -> Action -> FlashMessage -> H.Html
viewFlashMessage address action message =
  let empty = H.div [] []
      alert text class =
        H.div [HA.class ("alert alert-" ++ class ++ " alert-dismissible"), HA.stringProperty "role" "alert"]
          [ H.button [HE.onClick address action, HA.type' "button", HA.class "close", HA.stringProperty "aria-label" "Close"]
            [ H.span [] [ H.text "×" ] ]
          , H.text text
          ]
  in
    case message of
      Hidden -> empty
      Error m -> alert m "danger"

formatDate : Date.Date -> String
formatDate = DF.format "%d.%m.%Y um %H:%M Uhr"

view : Address Action -> Model -> H.Html
view address model =
  let header = viewFlashMessage address CloseFlashMessage model.flashMessage
      innerHeader = viewFlashMessage address CloseFlashMessage model.innerFlashMessage
      gigNavItem address currentGig gig =
        H.li (if gig == currentGig then [HA.class "disabled"] else []) [ H.a [ HA.href "#", HE.onClick address (ClickGig gig) ] [ H.text gig.title ] ]

  in
    case model.page of
      GigIndex ->
        H.div [HA.class "container"]
          [ H.div [HA.class "row"]
            [ H.div [HA.class "col-md-12"]
              [ H.h1 [] [H.text "Aufführungen"]
              , H.ul [HA.class "nav nav-pills nav-stacked"] (L.map (drawGigEntry address) model.gigs)
              ]
            ]
          ]
      GigView gig ->
        H.div [HA.class "container"]
          [ H.div [HA.class "row"]
            [ header ]
          , H.div [HA.class "row"]
            [ H.nav []
              [ H.ul [HA.class "pager"] (L.map (gigNavItem address gig) model.gigs) ]
            ]
          , H.div [HA.class "row"]
            [ H.div [HA.class "col-md-12"]
              [ H.h1 [] [H.text <| gig.title ++ " ", H.small [] [H.text <| formatDate gig.date]] ]
            ]
          , H.div [HA.class "row"]
            [ H.div [HA.class "col-md-12"]
              [ drawStand address model.stand ]
            ]
          , H.div [HA.class "row"]
            [ H.div [HA.class "col-md-12"]
              [ H.div [HA.class "panel panel-default"]
                [ H.div [HA.class "panel-heading"]
                  [ H.h3 [HA.class "panel-title"]
                    [ H.text "Karten bestellen" ]
                  ]
                , H.div [HA.class "panel-body"]
                  [ innerHeader
                  , viewOrderPanel address gig model
                  ]
                ]
              ]
            ]
          ]

viewOrderInfos : OrderInfo -> H.Html
viewOrderInfos order =
  H.h1 [] [ H.text <| "Karten für '" ++ order.name ++ "' ", H.small [] [H.text order.email] ]

viewOrderFinishForm : Address Action -> OrderInfo -> Model -> H.Html
viewOrderFinishForm address order model =
  let addressForm address model =
        case model.orderState of
          Ordering order ->
            case order.deliveryOption of
              Just (Delivery a) ->
                H.div []
                  [ H.div [HA.class "row"]
                    [ H.div [HA.class "col-md-12"]
                      [ textInput address UpdateStreet "street" "Strasse" a.street
                      ]
                    ]
                  , H.div [HA.class "row"]
                    [ H.div [HA.class "col-md-4"]
                      [ textInput address UpdatePostalCode "postalcode" "PLZ" a.postalCode
                      ]
                    , H.div [HA.class "col-md-8"]
                      [ textInput address UpdateCity "city" "Stadt" a.city
                      ]
                    ]
                  ]
              _ -> H.div [] []
          _ -> H.div [] []
  in
    H.div []
      [ H.div [HA.class "row"]
        [ H.div [HA.class "col-md-6"]
          [ numberInput address UpdateReducedCount "reduced" "davon ermäßigte Karten" model.formInput.reduced
          , radioInputs address UpdateDeliveryOption "deliveryOption" [PickUpBoxOffice, PickUpBeforehand, Delivery emptyAddress] PickUpBeforehand
          , addressForm address model
          ]
        ]
      , H.div [HA.class "row"]
        [ H.div [HA.class "col-md-12"]
          [ H.button [HE.onClick address (FinishOrderTicket order.id model.formInput.reduced), HA.class "btn btn-primary"]
            [ H.text "Bestellen"
            ]
          ]
        ]
      ]

viewOrderPanel : Address Action -> Gig -> Model -> H.Html
viewOrderPanel address gig model =
  case model.orderState of
    Ordering order ->
      H.div []
        [ viewOrderInfos order
        , viewOrderTable model
        , viewOrderFinishForm address order model
        ]
    Browsing ->
      H.div [HA.class "row"]
        [ H.div [HA.class "col-md-12"]
          [ viewRegisterForm address model.formInput ]
        ]


unwrapMaybe : Maybe x -> x
unwrapMaybe m =
  case m of
    Maybe.Just j -> j

reducedCount : Model -> Maybe Int
reducedCount model =
  Result.toMaybe <| String.toInt model.formInput.reduced

reducedCountValid : String -> Bool
reducedCountValid = isJust << Result.toMaybe << String.toInt

fullCount : Model -> Maybe Int
fullCount model =
  Maybe.map (\r -> (L.length model.stand.selections) - r) (reducedCount model)

reducedPrice : Model -> Maybe Price.Price
reducedPrice model =
  Maybe.map (\n -> Price.fromInt (n * 1200)) <| reducedCount model

fullPrice : Model -> Maybe Price.Price
fullPrice model =
  Maybe.map (\n -> Price.fromInt (n * 1600)) <| fullCount model

addPrice : Price.Price -> Price.Price -> Price.Price
addPrice = Price.add

combine : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
combine f a b = case a of
  Just x -> case b of
    Just y -> Just <| f x y
    Nothing -> Nothing
  Nothing -> Nothing

deliveryCosts : Model -> Price.Price
deliveryCosts model =
  case model.orderState of
    Ordering order ->
      case order.deliveryOption of
        Just (Delivery _) -> Price.fromInt 300
        _ -> Price.fromInt 0
    _ -> Price.fromInt 0


totalPrice : Model -> Maybe Price.Price
totalPrice model =
  Maybe.map (\price -> Price.add price (deliveryCosts model)) (combine addPrice (fullPrice model) (reducedPrice model))


mapWithDefault : (a -> b) -> b -> Maybe a -> b
mapWithDefault f d x =
  Maybe.withDefault d <| Maybe.map f x

viewOrderTable : Model -> H.Html
viewOrderTable model =
  let optionalDeliveryCosts = case model.orderState of
    Ordering order ->
      case order.deliveryOption of
        Just (Delivery _) ->
          [ H.tr []
            [ H.td [] [ H.text "Versandkosten" ]
            , H.td [HA.class "text-right"] [ H.text <| Price.format <| deliveryCosts model]
            ]
          ]
        _ -> []
    _ -> []
  in
    H.div []
      [ H.table [HA.class "table"]
        [ H.tbody []
          ([ H.tr []
            [ H.td [] [ H.text <| (mapWithDefault toString "-" <| fullCount model) ++ " reguläre Karten" ]
            , H.td [HA.class "text-right"] [ H.text <| mapWithDefault Price.format "-" <| fullPrice model ]
            ]
          , H.tr []
            [ H.td [] [ H.text <| (mapWithDefault toString "-" <| reducedCount model) ++ " ermäßigte Karten" ]
            , H.td [HA.class "text-right"] [ H.text <| mapWithDefault Price.format "-" <| reducedPrice model ]
            ]
          ] ++ optionalDeliveryCosts)
        , H.tfoot []
          [ H.tr []
            [ H.th [] [ H.strong [] [ H.text "Gesamtkosten" ] ]
            , H.th [HA.class "text-right"] [ H.strong [] [ H.text <| mapWithDefault Price.format "-" <| totalPrice model ] ]
            ]
          ]
        ]
      ]

-- form helpers
formInput : String -> Address Action -> (String -> Action) -> String -> String -> String -> H.Html
formInput type' address action name text value =
  H.div [HA.class "form-group"]
  [ H.label [HA.for name]
    [ H.text text]
  , H.input
      [ HA.type' type',
        HA.class "form-control",
        HA.id name,
        HA.value value,
        HE.on "input" HE.targetValue <| Signal.message address << action] []
  ]

textInput = formInput "text"
emailInput = formInput "email"
numberInput = formInput "number"

radioInputs : Address Action -> (DeliveryOption -> Action) -> String -> List DeliveryOption -> DeliveryOption -> H.Html
radioInputs address action name labels checked =
  let radioField option checked =
        H.div [HA.class "radio"]
          [ H.label []
            [ H.input [HE.onClick address (action option), HA.type' "radio", HA.name name, HA.checked (option == checked)] []
            , H.text <| labelFor option
            ]
          ]
      labelFor option = case option of
        PickUpBeforehand -> "Abholung vorab an der HGR"
        PickUpBoxOffice -> "Abholung an der Abendkasse"
        Delivery _ -> "Karten per Post zusenden lassen (Aufpreis von € 3,-)"
  in
    H.div [HA.class "form-group"] (L.map (\l -> radioField l checked) labels)

viewRegisterForm : Address Action -> CurrentFormInput -> H.Html
viewRegisterForm address form =
  H.div []
    [ textInput address UpdateName "name" "Name" form.name,
      emailInput address UpdateEmail "email" "E-Mail-Adresse" form.email,
      H.button [HE.onClick address (StartOrder form.name form.email), HA.class "btn btn-default"]
        [ H.text "Anmelden"
        ]
    ]

-- TODO remove selections by sliming down `OrderTicket`
viewTicketOrderForm : Gig -> Address Action -> CurrentFormInput -> (List M.Seat) -> H.Html
viewTicketOrderForm gig address form selections =
  H.div []
    [ textInput address UpdateName "name" "Name" form.name,
      emailInput address UpdateEmail "email" "E-Mail-Adresse" form.email,
      numberInput address UpdateReducedCount "reduced" "davon ermäßigte Karten" form.reduced,
      H.button [HE.onClick address (OrderTicket gig form.name form.email selections form.reduced), HA.class "btn btn-default"]
        [ H.text "Bestellen"
        ]
    ]


main : Signal H.Html
main =
  Signal.map (view actions.address) model

input : Signal Action
input = actions.signal

updatesWithEffect : Signal (Model, Maybe Effect)
updatesWithEffect =
  Signal.foldp update (initialModel, Nothing) input

isJust : Maybe a -> Bool
isJust m =
  case m of
    Just x -> True
    Nothing -> False

effects : Signal (Maybe Effect)
effects =
  Signal.filter isJust Nothing (Signal.map snd updatesWithEffect)

model : Signal Model
model =
  Signal.map fst updatesWithEffect

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp




-- SVG FOO

seatColor : M.Model -> M.Seat -> String
seatColor model seat =
  if | M.isSelected model seat -> "#0f0"
     | M.isReserved model seat -> "#f00"
     | otherwise -> "rgba(0,0,0,0)"

drawSeat : Address Action -> M.Model -> Int -> M.Seat -> S.Svg
drawSeat address model rowNumber seat =
  let text = if M.isUsable seat then toString seat.number else ""
  in
    S.g [HE.onClick address (ClickSeat seat), SA.transform <| "translate(" ++ (toString <| seat.x * 18) ++ ", 0)"]
      [ S.rect [SA.cursor "pointer", SA.style <| "fill:" ++ (seatColor model seat) ++ "; stroke-width: 1; stroke: rgb(0, 0, 0)", SA.width "18", SA.height "18"] []
      , S.text [SA.x "9", SA.y "13", SA.cursor "pointer", SA.textAnchor "middle", SA.fontSize "11"] [ H.text text]
      ]

drawRow : Address Action -> M.Model -> M.Row -> S.Svg
drawRow address model row = S.g [SA.transform <| "translate(0," ++ (toString <| row.y * 25) ++ ")"]
  [ S.g [SA.transform "translate(20, 0)"] (L.map (drawSeat address model row.number) (M.seatsInRow model row))
  , S.text [SA.x "5", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  , S.text [SA.x "880", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  ]


drawStand : Address Action -> M.Model -> H.Html
drawStand address model =
  S.svg [SA.version "1.1", SA.x "0", SA.y "0", SA.height "500", SA.width "910", SA.style "display: block; margin: 0 auto;"]
    [ S.g [SA.transform "translate(18, 1)"] (L.map (drawRow address model) (M.rows model))
    , S.line [SA.x1 "200", SA.y1 "450", SA.x2 "750", SA.y2 "450", SA.style "stroke:rgb(0,0,0);stroke-width:2"] []
    , S.text [SA.x "475", SA.y "475", SA.textAnchor "middle", SA.style "font-size: 16px"] [H.text "BÜHNE"]
    ]
