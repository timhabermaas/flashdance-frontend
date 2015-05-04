module Flashdance where

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Signal exposing (Address, Signal)
import List as L
import Text
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Color exposing (..)
import Maybe
import Time
import String
import Model as M
import Task.Extra as TE
import Http
import Task exposing (Task, andThen)
import HttpRequests exposing (..)

type alias GigId = String
type alias Name = String
type alias Email = String

type Action
  = NoOp
  | ClickSeat M.Seat
  | UpdateSeats (List M.Seat, List M.Row)
  | GigsReceived (List Gig)
  | ClickGig Gig
  | ReservationsReceived (List M.Reservation)
  | OrderTicket Gig Name Email (List M.Seat)
  | UpdateEmail String
  | UpdateName String
  | UpdateReducedCount String
  | HttpOrderFailed String
  | CloseFlashMessage

type Effect
  = FetchSeats GigId
  | SubmitOrder GigId Name Email (List M.Seat)

type CurrentPage = GigIndex | GigView Gig
type FlashMessage = Info String | Error String | Hidden
type alias CurrentFormInput = { name: String, email: String, reduced: String }
type alias Gig = { id: GigId, date: String, title: String, freeSeats: Int }
type alias Model = { formInput: CurrentFormInput, stand: M.Model, gigs: List Gig, page: CurrentPage, flashMessage: FlashMessage }

initialModel : Model
initialModel = { page = GigIndex, gigs = [], stand = M.initialModel, formInput = { name = "", email = "", reduced = "0"}, flashMessage = Hidden }

update : Action -> (Model, Maybe Effect) -> (Model, Maybe Effect)
update action (model, _) =
  case action of
    ClickSeat seat -> ({ model | stand <- M.toggleSelected model.stand seat}, Nothing)
    UpdateSeats (seats, rows) -> ({model | stand <- M.updateSeats model.stand seats rows}, Nothing)
    GigsReceived gigs -> ({model | gigs <- gigs}, Nothing)
    ReservationsReceived r -> ({model | stand <- M.updateReservations model.stand r}, Nothing)
    ClickGig gig -> ({model | page <- GigView gig}, Just <| FetchSeats gig.id)
    OrderTicket gig name email seats -> ({model | stand <- M.clearSelections <| M.reserveSeats model.stand seats}, Just <| SubmitOrder gig.id name email seats)
    UpdateEmail email -> ({model | formInput <- {name = model.formInput.name, email = email, reduced = model.formInput.reduced}}, Nothing)
    UpdateName name -> ({model | formInput <- {name = name, email = model.formInput.email, reduced = model.formInput.reduced}}, Nothing)
    UpdateReducedCount reduced -> ({model | formInput <- {name = model.formInput.name, email = model.formInput.email, reduced = reduced}}, Nothing)
    HttpOrderFailed error -> ({model | flashMessage <- (Error error)}, Nothing)
    CloseFlashMessage -> ({model | flashMessage <- Hidden}, Nothing)
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
        [(HttpRequests.fetchSeats id) `Task.andThen` (\r -> Signal.send actions.address (UpdateSeats r)),
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
    Just (SubmitOrder id name email seats) ->
      (HttpRequests.submitOrder id name email (List.map .id seats)
      `Task.andThen` (\result -> Signal.send actions.address NoOp))
      `Task.onError` (\error -> Signal.send actions.address (HttpOrderFailed (httpErrorToMessage error)))
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

viewFlashMessage : Address Action -> FlashMessage -> H.Html
viewFlashMessage address message =
  let empty = H.div [] []
      alert text class =
        H.div [HA.class ("alert alert-" ++ class ++ " alert-dismissible"), HA.stringProperty "role" "alert"]
          [ H.button [HE.onClick address CloseFlashMessage, HA.type' "button", HA.class "close", HA.stringProperty "aria-label" "Close"]
            [ H.span [] [ H.text "×" ] ]
          , H.text text
          ]
  in
    case message of
      Hidden -> empty
      Error m -> alert m "danger"

view : Address Action -> Model -> H.Html
view address model =
  let header = viewFlashMessage address model.flashMessage
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
            [ H.div [HA.class "col-md-12"]
              [ H.h1 [] [H.text "Gig"] ]
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
                    [ H.text "Tickets bestellen" ]
                  ]
                , H.div [HA.class "panel-body"]
                  [ H.div [HA.class "row"]
                    [ H.div [HA.class "col-md-6"]
                      [ H.h2 [] [ H.text "Reservierte Plätze" ]
                      , H.text <| M.selectionsAsText model.stand.selections
                      , viewOrderTable model
                      ]
                    , H.div [HA.class "col-md-6"]
                      [ viewTicketOrderForm gig address model.formInput model.stand.selections ]
                    ]
                  ]
                ]
              ]
            ]
          ]

unwrap : Result x y -> y
unwrap r = case r of
  Result.Ok s -> s

type Price = EUR Int

reducedCount : Model -> Maybe Int
reducedCount model =
  Result.toMaybe <| String.toInt model.formInput.reduced

fullCount : Model -> Maybe Int
fullCount model =
  Maybe.map (\r -> (L.length model.stand.selections) - r) (reducedCount model)

reducedPrice : Model -> Maybe Price
reducedPrice model =
  Maybe.map (\n -> EUR (n * 1200)) <| reducedCount model

fullPrice : Model -> Maybe Price
fullPrice model =
  Maybe.map (\n -> EUR (n * 1500)) <| fullCount model

addPrice : Price -> Price -> Price
addPrice (EUR x) (EUR y) = EUR <| x + y

combine : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
combine f a b = case a of
  Just x -> case b of
    Just y -> Just <| f x y
    Nothing -> Nothing
  Nothing -> Nothing

totalPrice : Model -> Maybe Price
totalPrice model =
  combine addPrice (fullPrice model) (reducedPrice model)

formatPrice : Price -> String
formatPrice p =
  case p of
    EUR x -> "€ " ++ toString ((toFloat x) / 100)

mapWithDefault : (a -> b) -> b -> Maybe a -> b
mapWithDefault f d x =
  Maybe.withDefault d <| Maybe.map f x

viewOrderTable : Model -> H.Html
viewOrderTable model =
  H.div []
    [ H.table [HA.class "table"]
      [ H.tbody []
        [ H.tr []
          [ H.td [] [ H.text <| (mapWithDefault toString "-" <| fullCount model) ++ " reguläre Karten" ]
          , H.td [HA.class "text-right"] [ H.text <| mapWithDefault formatPrice "-" <| fullPrice model ]
          ]
        , H.tr []
          [ H.td [] [ H.text <| (mapWithDefault toString "-" <| reducedCount model) ++ " ermäßigte Karten" ]
          , H.td [HA.class "text-right"] [ H.text <| mapWithDefault formatPrice "-" <| reducedPrice model ]
          ]
        ]
      , H.tfoot []
        [ H.tr []
          [ H.th [] [ H.strong [] [ H.text "Gesamtkosten" ] ]
          , H.th [HA.class "text-right"] [ H.strong [] [ H.text <| mapWithDefault formatPrice "-" <| fullPrice model ] ]
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

-- TODO remove selections by sliming down `OrderTicket`
viewTicketOrderForm : Gig -> Address Action -> CurrentFormInput -> (List M.Seat) -> H.Html
viewTicketOrderForm gig address form selections =
  H.div []
    [ textInput address UpdateName "name" "Name" form.name,
      emailInput address UpdateEmail "email" "E-Mail-Adresse" form.email,
      numberInput address UpdateReducedCount "reduced" "davon ermäßigte Karten" form.reduced,
      H.button [HE.onClick address (OrderTicket gig form.name form.email selections), HA.class "btn btn-default"]
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
  if | M.isSelected model seat -> "#f00"
     | M.isReserved model seat -> "#0f0"
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
  , S.text [SA.x "1023", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  ]


drawStand : Address Action -> M.Model -> H.Html
drawStand address model = S.svg [SA.version "1.1", SA.x "0", SA.y "0", SA.height "480", SA.width "1050"]
  [ S.g [SA.transform "translate(18, 1)"]
    (L.map (drawRow address model) (M.rows model))
  ]
