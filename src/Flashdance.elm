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
  | OrderSucceeded
  | ResetApp
  | ClickSeat M.Seat
  | SeatsReceived (List M.Seat, List M.Row)
  | GigsReceived (List Gig)
  | OrdersReceived (List Order)
  | PayOrder Order
  | ClickGig Gig
  | ClickOrder Order
  | ClickPaid Order
  | ClickUnpaid Order
  | ClickCancelOrder Order
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
  | TypeSearch String
  | ShowErrorFlashMessage String
  | UpdateDeliveryOption DeliveryOption
  | FinishOrderTicket OrderId String
  | ClickAdmin
  | UpdateLoginPassword String
  | UpdateLoginName String
  | Login Credentials
  | LoginAsUserSucceeded Credentials
  | LoginAsAdminSucceeded Credentials

type Effect
  = FetchSeats GigId
  | SubmitOrder GigId Name Email (List M.Seat) Int
  | StartOrderRequest Name Email
  | ReserveSeatRequest OrderId SeatId
  | FreeSeatRequest OrderId SeatId
  | FinishOrderRequest OrderId DeliveryOption Int String
  | FinishOrderWithAddressRequest OrderId Int Address'
  | LoginRequest Credentials
  | PaidRequest Credentials Order
  | ListOrderRequest Credentials

type CurrentPage = GigIndex | GigView Gig
type FlashMessage = Success String | Info String | Error String | Hidden
type DeliveryOption = PickUpBoxOffice | PickUpBeforehand | Delivery Address'
type alias OrderInfo = { name: String, email: String, id: OrderId, deliveryOption: DeliveryOption }
type OrderState = Ordering OrderInfo | Browsing | Ordered OrderInfo
type alias Address' = { street: String, postalCode: String, city: String }
type alias CurrentFormInput = { name: String, email: String, reduced: String }
type alias Gig = { id: GigId, date: Date.Date, title: String, freeSeats: Int }
type alias Order = { id: OrderId, createdAt: Date.Date, name: String, email: String, paid: Bool, reducedCount: Int, seatIds: List SeatId, number: Int, address: Maybe Address'}
type alias Credentials = { name: String, password: String }
type Session = Anonymous | User Credentials | Admin Credentials
type alias Model = { formInput: CurrentFormInput, stand: M.Model, gigs: List Gig, orderState: OrderState, page: CurrentPage, flashMessage: FlashMessage, innerFlashMessage: FlashMessage, loginFields: Maybe Credentials, session: Session, orders: List Order, currentOrder: Maybe Order, searchField: String }

emptyAddress : Address'
emptyAddress = { street = "", postalCode = "", city = "" }

initialModel : Model
initialModel = { page = GigIndex, gigs = [], stand = M.initialModel, formInput = { name = "", email = "", reduced = "0"}, flashMessage = Hidden, orderState = Browsing, innerFlashMessage = Hidden, loginFields = Nothing, session = Anonymous, orders = [], currentOrder = Nothing, searchField = "" }

updateAddress : Model -> Address' -> Model
updateAddress model address =
  case model.orderState of
    Ordering order ->
      case order.deliveryOption of
        Delivery a -> {model | orderState <- Ordering ({order | deliveryOption <- Delivery address})}
        _ -> model
    _ -> model

forcefullyExtractDeliveryOption : Model -> DeliveryOption
forcefullyExtractDeliveryOption model =
  case model.orderState of
    Ordering order -> order.deliveryOption

forcefullyExtractOrderInfo : Model -> OrderInfo
forcefullyExtractOrderInfo model =
  case model.orderState of
    Ordering order -> order

forcefullyExtractAddress : Model -> Address'
forcefullyExtractAddress model =
  case model.orderState of
    Ordering order ->
      case order.deliveryOption of
        Delivery a -> a

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
    OrderSucceeded -> ({model | orderState <- Ordered (forcefullyExtractOrderInfo model)}, Nothing)
    ResetApp -> ({model | orderState <- Browsing, stand <- M.clearSelections <| M.reserveSeats model.stand model.stand.selections, flashMessage <- Hidden, formInput <- { name = "", email = "", reduced = "0"}, innerFlashMessage <- Hidden}, Nothing)
    SeatReserved seatId -> ({model | stand <- M.reserveSeats model.stand [unwrapMaybe <| M.findSeat model.stand seatId]}, Nothing)
    SeatSelected seatId -> ({model | stand <- M.selectSeat model.stand <| unwrapMaybe <| M.findSeat model.stand seatId}, Nothing)
    SeatUnselected seatId -> ({model | stand <- M.unselectSeat model.stand <| unwrapMaybe <| M.findSeat model.stand seatId}, Nothing)
    SeatsReceived (seats, rows) ->
      case model.orderState of
        Ordering _ -> ({model | stand <- M.updateSeats model.stand seats rows}, Nothing)
        _ ->
          case model.currentOrder of
            Just o -> ({model | stand <- M.selectSeatIds (M.updateSeats model.stand seats rows) o.seatIds}, Nothing)
            Nothing -> ({model | stand <- M.updateSeats model.stand seats rows}, Nothing)
    GigsReceived gigs -> ({model | gigs <- gigs}, Nothing)
    PayOrder order -> ({model | orders <- ordersWithPaid model.orders order, currentOrder <- Just {order | paid <- True}}, Nothing)
    ReservationsReceived r -> ({model | stand <- M.updateReservations model.stand r}, Nothing)
    ClickGig gig -> ({model | page <- GigView gig}, Just <| FetchSeats gig.id)
    ClickOrder order ->
      case model.orderState of
        Ordering _ -> ({model | currentOrder <- Just order}, Nothing)
        _ -> ({model | currentOrder <- Just order, stand <- M.selectSeatIds (M.clearSelections model.stand) order.seatIds}, Nothing)

    OrderTicket gig name email seats reduced ->
      case String.toInt reduced of
        Result.Ok x -> ({model | stand <- M.clearSelections <| M.reserveSeats model.stand seats}, Just <| SubmitOrder gig.id name email seats x)
        Result.Err x -> (model, Nothing)
    UpdateEmail email -> ({model | formInput <- {name = model.formInput.name, email = email, reduced = model.formInput.reduced}}, Nothing)
    UpdateName name -> ({model | formInput <- {name = name, email = model.formInput.email, reduced = model.formInput.reduced}}, Nothing)
    UpdateDeliveryOption option ->
      case model.orderState of
        Browsing -> (model, Nothing)
        Ordering order -> ({model | orderState <- Ordering ({name = order.name, email = order.email, id = order.id, deliveryOption = option})}, Nothing)
    UpdateReducedCount reduced ->
      if reducedCountValid reduced (List.length model.stand.selections) then
        -- TODO needs more sophistacted approach, we reset the error field even if there might still be errors
        ({model | innerFlashMessage <- Hidden, formInput <- {name = model.formInput.name, email = model.formInput.email, reduced = reduced}}, Nothing)
      else
        ({model | innerFlashMessage <- Error "Bitte gültige Anzahl an ermäßigten Karten angeben.", formInput <- {name = model.formInput.name, email = model.formInput.email, reduced = reduced}}, Nothing)
    UpdateCity city -> (updateAddress model (updateCity (forcefullyExtractAddress model) city), Nothing)
    UpdateStreet street -> (updateAddress model (updateStreet (forcefullyExtractAddress model) street), Nothing)
    UpdatePostalCode pc -> (updateAddress model (updatePostalCode (forcefullyExtractAddress model) pc), Nothing)
    HttpOrderFailed error -> ({model | flashMessage <- (Error error)}, Nothing)
    CloseFlashMessage -> ({model | flashMessage <- Hidden}, Nothing)
    TypeSearch s -> ({model | searchField <- s}, Nothing)
    StartOrder name email ->
      if startOrderValid model name email then
        ({ model | stand <- M.clearSelections model.stand}, Just <| StartOrderRequest name email)
      else
        ({ model | innerFlashMessage <- Error "Name oder E-Mail nicht vorhanden."}, Nothing)

    FinishOrderTicket orderId reducedCountAsString ->
      let request =
            case (forcefullyExtractDeliveryOption model) of
              Delivery _ -> FinishOrderWithAddressRequest orderId (unwrapMaybe <| reducedCount model) (forcefullyExtractAddress model)
              _ -> FinishOrderRequest orderId (forcefullyExtractDeliveryOption model) (unwrapMaybe <| reducedCount model) (typeFromDeliveryOption (forcefullyExtractDeliveryOption model))
      in
        (model, Just <| request)
    OrderStarted orderId -> ({model | innerFlashMessage <- Hidden, orderState <- Ordering {name = model.formInput.name, email = model.formInput.email, id = orderId, deliveryOption = PickUpBoxOffice}}, Nothing)
    ShowErrorFlashMessage message -> ({model | flashMessage <- Error message}, Nothing)
    ClickAdmin -> ({model | loginFields <- Just {name = "", password = ""}}, Nothing)
    UpdateLoginName name -> ({model | loginFields <- Just {name = name, password = .password <| unwrapMaybe model.loginFields}}, Nothing)
    UpdateLoginPassword pw -> ({model | loginFields <- Just {name = .name <| unwrapMaybe model.loginFields, password = pw}}, Nothing)
    Login credentials -> (model, Just <| LoginRequest credentials)
    LoginAsUserSucceeded credentials -> ({model | session <- User credentials}, Nothing)
    LoginAsAdminSucceeded credentials -> ({model | session <- Admin credentials}, Just <| ListOrderRequest credentials)
    OrdersReceived orders -> ({model | orders <- orders}, Nothing)
    ClickPaid order ->
      case model.session of
        Admin c -> (model, Just <| PaidRequest c order)
        _ -> (model, Nothing)
    NoOp -> (model, Nothing)

startOrderValid : Model -> String -> String -> Bool
startOrderValid model name email =
  case model.session of
    Anonymous -> (String.contains "@" email) && not (String.isEmpty name)
    _ -> not (String.isEmpty name)

ordersWithPaid : List Order -> Order -> List Order
ordersWithPaid orders order =
  L.map (\o -> if o.id == order.id then {o | paid <- True} else o) orders


filterOrders : List Order -> String -> List Order
filterOrders orders search =
  if String.isEmpty search then
    orders
  else
    case String.toInt search of
      Ok n -> List.filter (\o -> String.contains (toString n) (toString o.number)) orders
      Err _ -> List.filter (\o -> String.contains (String.toLower search) (String.toLower o.name)) orders


port fetchGigs : Task Http.Error ()
port fetchGigs =
  HttpRequests.fetchGigs `Task.andThen` (\gigs -> Signal.send actions.address (GigsReceived gigs))

port fetchOrders : Signal (Task Http.Error ())
port fetchOrders =
  let maybeRequest s = case s of
    Just (ListOrderRequest credentials) ->
      HttpRequests.orders `Task.andThen` (\orders -> Signal.send actions.address (OrdersReceived orders))
      `Task.onError` (\error -> Signal.send actions.address (ShowErrorFlashMessage <| toString error))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects


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


typeFromDeliveryOption : DeliveryOption -> String
typeFromDeliveryOption do =
  case do of
    PickUpBeforehand -> "pickUpBeforehand"
    PickUpBoxOffice -> "pickUpBoxOffice"

port finishOrderRequest : Signal (Task Http.Error ())
port finishOrderRequest =
  let maybeRequest s = case s of
        Just (FinishOrderRequest orderId deliveryOption reducedCount type') ->
          (HttpRequests.finishOrder orderId reducedCount (typeFromDeliveryOption deliveryOption))
          `Task.andThen` (\_ -> Signal.send actions.address OrderSucceeded)
        Just (FinishOrderWithAddressRequest orderId reducedCount address) ->
          (HttpRequests.finishOrderWithAddress orderId reducedCount address.street address.postalCode address.city)
          `Task.andThen` (\_ -> Signal.send actions.address OrderSucceeded)
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

port paidRequest : Signal (Task Http.Error ())
port paidRequest =
  let maybeRequest s = case s of
    Just (PaidRequest credentials order) ->
      (HttpRequests.paid order.id credentials.name credentials.password)
      `Task.andThen` (\_ -> Signal.send actions.address (PayOrder order))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects

port loginRequest : Signal (Task Http.Error ())
port loginRequest =
  let maybeRequest s = case s of
    Just (LoginRequest credentials) ->
      (HttpRequests.login credentials.name credentials.password)
      `Task.andThen` (\result -> Signal.send actions.address (if result == "admin" then LoginAsAdminSucceeded credentials else LoginAsUserSucceeded credentials))
      --`Task.onError` (\result -> (Signal.send actions.address (ShowErrorFlashMessage "Sitz konnte nicht entfernt werden.")))
    _ -> Task.succeed ()
  in Signal.map maybeRequest effects

emptyHref = HA.href "javascript:void(0)"

drawGigEntry : Address Action -> Gig -> H.Html
drawGigEntry address gig =
  H.li []
    [ H.a [emptyHref, HE.onClick address (ClickGig gig)]
      [ H.text <| (formatDate gig.date) ++ " "
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
      Success m -> alert m "success"
      Info m -> alert m "info"

formatDateTime : Date.Date -> String
formatDateTime = DF.format "%d.%m.%Y um %H:%M Uhr"

formatDate : Date.Date -> String
formatDate date =
  let monthName = "Juli"
      day = DF.format "%d"
      year = DF.format "%Y"
  in  (day date) ++ ". " ++ monthName ++ " " ++ (year date)

formatShortDate : Date.Date -> String
formatShortDate = DF.format "%d.%m.%Y"

viewOrderList : Address Action -> Model -> H.Html
viewOrderList address model =
  let paidLabel order =
        H.span [HA.class ("label " ++ (if order.paid then "label-success" else "label-warning"))] [ H.text (if order.paid then "Bezahlt" else "Nicht bezahlt") ]
      countLabel order =
        H.span [HA.class "label label-success"] [ H.text <| toString <| List.length order.seatIds]
      orderItem currentOrder order =
        H.li [HE.onClick address (ClickOrder order), HA.style [("cursor", "pointer")], HA.class <| "list-group-item" ++ (if (Just order) == currentOrder then " active" else "")]
          [ H.span [HA.class "badge"] [ H.text <| "am " ++ formatShortDate order.createdAt ]
          , H.text <| "#" ++ (toString order.number) ++ " " ++ order.name
          , paidLabel order
          , countLabel order
          ]
  in
      H.div []
        [ H.div [HA.class "row"]
          [ H.div [HA.class "form-group"]
            [ H.div [HA.class "input-group"]
              [ H.div [HA.class "input-group-addon"]
                [ H.span [HA.class "glyphicon glyphicon-search"] []
                ]
              , H.input [HE.on "input" HE.targetValue <| Signal.message address << TypeSearch, HA.type' "text", HA.class "form-control", HA.placeholder "Suche..."] []
              ]
            ]
          ]
        , H.div [HA.class "row"]
          [ H.ul [HA.class "list-group"]
              (L.map (orderItem model.currentOrder) (filterOrders model.orders model.searchField))
          ]
        ]

viewOrderDetail : Address Action -> Model -> H.Html
viewOrderDetail address model =
  let paidButton order =
        if not order.paid then
          H.button [HE.onClick address (ClickPaid order), HA.class "btn btn-primary"]
            [ H.text "Bezahlt!"
            ]
        else
          H.text ""
          --H.button [HE.onClick address (ClickUnpaid order), HA.class "btn btn-warning"]
            --[ H.text "Bezahlung widerrufen!"
            --]
      cancelButton order =
        H.button [HE.onClick address (ClickCancelOrder order), HA.class "btn btn-danger"]
          [ H.text "Bestellung stornieren"
          ]
      addressList address =
        case address of
          Just a ->
            H.dl []
              [ H.dt [] [H.text "Straße"]
              , H.dd [] [H.text a.street]
              , H.dt [] [H.text "PLZ"]
              , H.dd [] [H.text a.postalCode]
              , H.dt [] [H.text "Stadt"]
              , H.dd [] [H.text a.city]
              ]
          Nothing ->
            H.text ""
  in
      case model.currentOrder of
        Nothing ->
          H.text ""
        Just order ->
            H.div [HA.class "panel panel-default"]
              [ H.div [HA.class "panel-heading"]
                [ H.h3 [HA.class "panel-title"]
                  [ H.text "Bestellungsdetail" ]
                ]
              , H.div [HA.class "panel-body"]
                [ H.h2 [] [H.text order.name, H.small [] [H.text <| " #" ++ toString order.number]]
                , viewOrderTable' order
                , paidButton order
                , cancelButton order
                , addressList order.address
                ]
              ]

isAdmin : Session -> Bool
isAdmin s =
  case s of
    Admin _ -> True
    _ -> False


view : Address Action -> Model -> H.Html
view address model =
  let header = viewFlashMessage address CloseFlashMessage model.flashMessage
      innerHeader = viewFlashMessage address CloseFlashMessage model.innerFlashMessage
      gigNavItem address currentGig gig =
        H.li (if gig == currentGig then [HA.class "disabled"] else []) [ H.a [ emptyHref, HE.onClick address (ClickGig gig) ] [ H.text (formatDate gig.date) ] ]
      body model =
        case model.page of
          GigIndex ->
            [ H.div [HA.class "row"]
              [ H.div [HA.class "col-md-12"]
                [ H.h1 [] [H.text "FLASHDANCE – The Musical | Tickets"]
                , H.br [] []
                , H.p []
                  [ H.text "Herzlich Willkommen beim Online-Ticketservice der HGR Musical AG!"
                  , H.br [] []
                  , H.text "Bitte wählen Sie einen Veranstaltungstag, um zu beginnen."
                  ]
                ]
              ]
            , H.br [] []
            , H.div [HA.class "row"]
              [ H.div [HA.class "col-md-4"]
                [ H.ul [HA.class "nav nav-pills nav-stacked"] (L.map (drawGigEntry address) model.gigs)
                ]
              , H.div [HA.class "col-md-8"]
                [ H.img [HA.src "logo.png"] []
                ]
              ]
            , H.div [HA.class "row"]
              [ H.div [HA.class "col-md-12"]
                [ H.br [] []
                , H.p [] [H.text "What a feeling: Wir haben unsere Zuschauertribüne überarbeitet und bieten ab sofort 20% mehr Sitzfreiheit je Platz!"]
                ]
              ]
            ]
          GigView gig ->
            [ H.div [HA.class "row"]
              [ header ]
            , H.div [HA.class "row"]
              [ H.nav []
                [ H.ul [HA.class "pager"] (L.map (gigNavItem address gig) model.gigs) ]
              ]
            , H.div [HA.class "row"]
              [ H.div [HA.class "col-md-12"]
                [ H.h1 [] [H.text <| gig.title ++ " ", H.small [] [H.text <| formatDateTime gig.date]] ]
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
            , (if (isAdmin model.session) then H.div [HA.class "row"]
              [ H.div [HA.class "col-md-4"]
                [ viewOrderList address model
                ]
              , H.div [HA.class "col-md-8"]
                [ viewOrderDetail address model
                ]
              ] else H.text "")
            ]
      loginForm model =
        case model.session of
          Anonymous ->
            case model.loginFields of
              Nothing ->
                H.form [HA.class "nav navbar-form navbar-right"]
                [ H.button [HE.onClick address ClickAdmin, HA.type' "button", HA.class "btn btn-default"] [H.text "Admin"]
                ]
              Just fields ->
                H.form [HA.class "navbar-form navbar-right"]
                [ textInput address UpdateLoginName "user" "Username" fields.name
                , passwordInput address UpdateLoginPassword "password" "Passwort" fields.password
                , H.button [HE.onClick address (Login fields), HA.type' "button", HA.class "btn btn-primary"] [H.text "Login"]
                ]
          Admin credentials ->
            H.p [HA.class "navbar-text navbar-right"]
            [ H.text "Eingeloggt als Admin" ]
          User credentials ->
            H.p [HA.class "navbar-text navbar-right"]
            [ H.text "Eingeloggt als Benutzer" ]

      nav model =
        H.nav [HA.class "navbar navbar-default"]
        [ H.div [HA.class "container-fluid"]
          [ H.div [HA.class "navbar-header"]
            [ H.a [HA.class "navbar-brand"] [H.text "FLASHDANCE"]
            ]
          , H.div [HA.class "collapse navbar-collapse"]
            [ loginForm model
            ]
          ]
        ]

  in
     H.div [HA.class "container"] ([nav model] ++ (body model))


viewOrderInfos : OrderInfo -> H.Html
viewOrderInfos order =
  H.h1 [] [ H.text <| "Karten für '" ++ order.name ++ "' ", H.small [] [H.text order.email] ]

viewOrderFinishForm : Address Action -> OrderInfo -> Model -> H.Html
viewOrderFinishForm address order model =
  let addressForm address model =
        case model.orderState of
          Ordering order ->
            case order.deliveryOption of
              Delivery a ->
                H.div []
                  [ H.div [HA.class "row"]
                    [ H.div [HA.class "col-md-12"]
                      [ textInput address UpdateStreet "street" "Straße" a.street
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
          [ H.button [HA.disabled <| not (reducedCountValid model.formInput.reduced (List.length model.stand.selections)), HE.onClick address (FinishOrderTicket order.id model.formInput.reduced), HA.class "btn btn-primary"]
            [ H.text "Bestellen"
            ]
          ]
        ]
      ]

isDelivery : DeliveryOption -> Bool
isDelivery do =
  case do of
    Delivery _ -> True
    _ -> False



viewOrderPanel : Address Action -> Gig -> Model -> H.Html
viewOrderPanel address gig model =
  case model.orderState of
    Ordering order ->
      H.div []
        [ viewOrderInfos order
        , H.div []
          [ H.br [] []
          , H.p []
            [ H.text "Nachdem Sie alle Sitzplätze ausgewählt und die Anzahl der ermäßigten Karten (Schüler und Studenten mit gültigem Ausweis) angegeben haben,
klicken Sie zum Abschließen Ihrer Bestellung auf „Absenden“. Sie erhalten dann in Kürze eine Bestätigungs-E-Mail mit den Zahlungsinformationen."
            ]
          , H.br [] []
          ]
        , viewOrderTable model
        , viewOrderFinishForm address order model
        ]
    Browsing ->
      H.div [HA.class "row"]
        [ H.div [HA.class "col-md-12"]
          [ H.p []
            [ H.br [] []
            , H.text "Bitte geben Sie Ihren Namen und eine gültige E-Mail-Adresse ein, um Sitzplätze per Mausklick auszuwählen."
            , H.br [] []
            , H.text "Nachdem Sie sich angemeldet haben, können Sie zudem weitere Veranstaltungstage zu Ihrer Bestellung hinzufügen."
            ]
          , H.br [] []
          , viewRegisterForm address model.formInput
          ]
        ]
    Ordered order ->
      H.div [HA.class "row"]
        [ H.div [HA.class "col-md-12"]
          [ viewFlashMessage address NoOp (Success "Karten erfolgreich bestellt.")
          , H.button [HE.onClick address ResetApp, HA.class "btn btn-primary"]
            [ H.text "Weitere Karten bestellen"
            ]
          ]
        ]


unwrapMaybe : Maybe x -> x
unwrapMaybe m =
  case m of
    Maybe.Just j -> j

reducedCount : Model -> Maybe Int
reducedCount model =
  Result.toMaybe <| String.toInt model.formInput.reduced

reducedCountValid : String -> Int -> Bool
reducedCountValid reduced totalCount =
  case String.toInt reduced of
    Ok x -> totalCount - x >= 0 && x >= 0 && totalCount > 0
    Err _ -> False

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
        Delivery _ -> Price.fromInt 300
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
        Delivery _ ->
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

type alias OrderTable = { fullCount : Int,
                          fullPrice : Price.Price,
                          reducedCount : Int,
                          reducedPrice : Price.Price,
                          deliveryPrice : Maybe Price.Price,
                          totalPrice : Price.Price
                        }



orderTableFromOrder : Order -> OrderTable
orderTableFromOrder order =
  let fullCount = (List.length order.seatIds) - order.reducedCount
      fullPrice = Price.fromInt <| 1600 * fullCount
      reducedPrice = Price.fromInt <| 1200 * order.reducedCount
      deliveryPrice = (if isJust order.address then Just <| Price.fromInt 300 else Nothing)
  in
      { fullCount = fullCount,
        fullPrice = fullPrice,
        reducedCount = order.reducedCount,
        reducedPrice = reducedPrice,
        totalPrice =
          case deliveryPrice of
            Just p -> Price.add p (Price.add fullPrice reducedPrice)
            Nothing -> Price.add fullPrice reducedPrice,
        deliveryPrice = deliveryPrice
      }




viewOrderTable' : Order -> H.Html
viewOrderTable' order =
  let orderTable = orderTableFromOrder order
      optionalDeliveryCosts = case order.address of
        Just _ ->
          [ H.tr []
            [ H.td [] [ H.text "Versandkosten" ]
            , H.td [HA.class "text-right"] [ H.text <| Price.format <| unwrapMaybe <| orderTable.deliveryPrice]
            ]
          ]
        _ -> []
  in
    H.div []
      [ H.table [HA.class "table"]
        [ H.tbody []
          ([ H.tr []
            [ H.td [] [ H.text <| (toString <| orderTable.fullCount) ++ " reguläre Karten" ]
            , H.td [HA.class "text-right"] [ H.text <| Price.format <| orderTable.fullPrice ]
            ]
          , H.tr []
            [ H.td [] [ H.text <| (toString <| orderTable.reducedCount) ++ " ermäßigte Karten" ]
            , H.td [HA.class "text-right"] [ H.text <| Price.format <| orderTable.reducedPrice ]
            ]
          ] ++ optionalDeliveryCosts)
        , H.tfoot []
          [ H.tr []
            [ H.th [] [ H.strong [] [ H.text "Gesamtkosten" ] ]
            , H.th [HA.class "text-right"] [ H.strong [] [ H.text <| Price.format <| orderTable.totalPrice ] ]
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
passwordInput = formInput "password"
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
        PickUpBeforehand -> "Abholung vorab an der HGR (Mo. – Fr., 13.00 – 14.30 Uhr, Raum 234)"
        PickUpBoxOffice -> "Abholung an der Abendkasse"
        Delivery _ -> "Karten per Post zusenden lassen (Aufpreis von € 3,-)"
  in
    H.div [HA.class "form-group"] (L.map (\l -> radioField l checked) labels)

viewRegisterForm : Address Action -> CurrentFormInput -> H.Html
viewRegisterForm address form =
  H.div []
    [ textInput address UpdateName "name" "Name, Vorname" form.name,
      emailInput address UpdateEmail "email" "E-Mail-Adresse" form.email,
      H.button [HE.onClick address (StartOrder form.name form.email), HA.class "btn btn-primary"]
        [ H.text "Anmelden"
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
