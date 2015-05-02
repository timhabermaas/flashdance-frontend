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
import Http
import Task exposing (Task, andThen)
import Json.Decode exposing (..)
import Json.Encode exposing (..)

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

type Effect
  = FetchSeats GigId
  | SubmitOrder GigId Name Email (List M.Seat)

type CurrentPage = GigIndex | GigView Gig
type alias CurrentFormInput = { name: String, email: String }
type alias Gig = { id: GigId, date: String, title: String }
type alias Model = { formInput: CurrentFormInput, stand: M.Model, gigs: List Gig, page: CurrentPage }

baseApiEndpoint : String
baseApiEndpoint = "https://tickets-backend-ruby.herokuapp.com"

initialModel : Model
initialModel = { page = GigIndex, gigs = [], stand = M.initialModel, formInput = { name = "", email = ""}}

currentGig : CurrentPage -> Maybe Gig
currentGig page = case page of
  GigView p -> Just p
  _ -> Nothing

fromJust : Maybe a -> a
fromJust x = case x of
  Just v -> v

update : Action -> (Model, Maybe Effect) -> (Model, Maybe Effect)
update action (model, _) =
  case action of
    ClickSeat seat -> ({ model | stand <- M.toggleSelected model.stand seat}, Nothing)
    UpdateSeats (seats, rows) -> ({model | stand <- M.updateSeats model.stand seats rows}, Nothing)
    GigsReceived gigs -> ({model | gigs <- gigs}, Nothing)
    ReservationsReceived r -> ({model | stand <- M.updateReservations model.stand r}, Nothing)
    ClickGig gig -> ({model | page <- GigView gig}, Just <| FetchSeats gig.id)
    OrderTicket gig name email seats -> ({model | stand <- M.clearSelections <| M.reserveSeats model.stand seats}, Just <| SubmitOrder gig.id name email seats)
    NoOp -> (model, Nothing)



gigDecoder : Decoder (List Gig)
gigDecoder =
  Json.Decode.list (object3 Gig ("id" := Json.Decode.string) ("date" := Json.Decode.string) ("title" := Json.Decode.string))

port fetchGigs : Task Http.Error ()
port fetchGigs =
  Http.get gigDecoder (baseApiEndpoint ++ "/gigs") `Task.andThen` (\gigs -> Signal.send actions.address (GigsReceived gigs))

seatDecoder : Decoder M.Seat
seatDecoder =
  object5 M.Seat
    ("id" := Json.Decode.string)
    ("x" := Json.Decode.int)
    ("number" := Json.Decode.int)
    ("row" := Json.Decode.int)
    ("usable" := Json.Decode.bool)

rowDecoder : Decoder M.Row
rowDecoder =
  object2 M.Row
    ("number" := Json.Decode.int)
    ("y" := Json.Decode.int)

seatsDecoder : Decoder (List M.Seat, List M.Row)
seatsDecoder =
  object2 (,)
    ("seats" := (Json.Decode.list seatDecoder))
    ("rows" := (Json.Decode.list rowDecoder))

reservationDecoder : Decoder M.Reservation
reservationDecoder =
  object1 M.Reservation ("seatId" := Json.Decode.string)

reservationsDecoder : Decoder (List M.Reservation)
reservationsDecoder = Json.Decode.list reservationDecoder

seatsRequestGigIdSignal : Signal (Maybe GigId)
seatsRequestGigIdSignal =
  let toId effect =
    case effect of
      Just (FetchSeats id) -> Just id
      _ -> Nothing
  in
    Signal.map toId effects


port seatsRequest : Signal (Task Http.Error (List ()))
port seatsRequest =
  let send gigId = case gigId of
    Just id ->
      Task.sequence [(Http.get seatsDecoder (baseApiEndpoint ++ "/gigs/" ++ id ++ "/seats")) `Task.andThen` (\r -> Signal.send actions.address (UpdateSeats r)),
       (Http.get reservationsDecoder (baseApiEndpoint ++ "/gigs/" ++ id ++ "/reservations")) `Task.andThen` (\r -> Signal.send actions.address (ReservationsReceived r))
      ]
    Nothing -> Task.succeed [()]

  in Signal.map send seatsRequestGigIdSignal

port orderRequest : Signal (Task Http.Error (List String))
port orderRequest =
  let maybeRequest s = case s of
    Just (SubmitOrder id name email seats) -> Http.post (Json.Decode.list Json.Decode.string) (baseApiEndpoint ++ "/gigs/" ++ id ++ "/orders") (Http.string (Json.Encode.encode 0 (object [("name", Json.Encode.string name), ("email", Json.Encode.string email), ("seatIds", Json.Encode.list (List.map Json.Encode.string (List.map .id seats)))])))
    _ -> Task.succeed []
  in Signal.map maybeRequest effects



drawGigEntry : Address Action -> Gig -> H.Html
drawGigEntry address gig =
  H.li []
    [ H.a [HA.href "#", HE.onClick address (ClickGig gig)]
      [ H.text gig.title
      , H.span [HA.class "badge"] [H.text "2 freie Plätze"]
      ]
    ]

view : Address Action -> Model -> H.Html
view address model =
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
    GigView _ ->
      H.div [HA.class "container"]
        [ H.div [HA.class "row"]
          [ H.div [HA.class "col-md-12"]
            [ H.h1 [] [H.text "Gig"] ]
          ]
        , H.div [HA.class "row"]
          [ H.div [HA.class "col-md-12"]
            [ drawStand address model.stand
            , H.text <| M.selectionsAsText model.stand
            , H.a [HA.href "#", HE.onClick address (OrderTicket (fromJust (currentGig model.page)) "foo" "bar@gmail.com" model.stand.selections)]
              [ H.text "Bestellen!"
              ]
            ]
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
