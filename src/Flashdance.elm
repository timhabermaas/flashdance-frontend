module Flashdance where

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Signal
import List as L
import Text
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Collage (..)
import Color (..)
import Maybe
import Time
import String
import Model as M

type alias GigId = String
type Action = NoOp | ClickSeat M.Seat | UpdateSeats (List M.Seat, List M.Row) | GigsReceived (List Gig) | ClickGig Gig
type Effect = FetchSeats GigId


type CurrentPage = GigIndex | GigView Gig
type alias Model = { stand: M.Model, gigs: List Gig, page: CurrentPage }

initialModel : Model
initialModel = { page = GigIndex, gigs = [], stand = M.initialModel }


update : Action -> (Model, Maybe Effect) -> (Model, Maybe Effect)
update action (model, _) =
  case action of
    ClickSeat seat -> ({ model | stand <- M.toggleSelected model.stand seat}, Nothing)
    UpdateSeats (seats, rows) -> ({model | stand <- M.updateSeats model.stand seats rows}, Nothing)
    GigsReceived gigs -> ({model | gigs <- gigs}, Nothing)
    ClickGig gig -> ({model | page <- GigView gig}, Just <| FetchSeats gig.id)
    NoOp -> (model, Nothing)

port requests : Signal String
port requests = fooRequest

type alias SeatsResponse = { seats: List M.Seat, rows: List M.Row }
type alias Gig = { id: GigId, date: String, title: String }
type alias GigsResponse = List Gig

port seatsReceived : Signal SeatsResponse
port gigsReceived : Signal GigsResponse

responseToAction : SeatsResponse -> Action
responseToAction r = UpdateSeats (r.seats, r.rows)

responseToAction2 : GigsResponse -> Action
responseToAction2 r = GigsReceived r


toRequest : Maybe Effect -> GigId
toRequest e = case e of
  (Just (FetchSeats s)) -> s
  Nothing -> ""

fooRequest : Signal String
fooRequest = Signal.map (snd >> toRequest) updatesWithEffect |> Signal.dropIf String.isEmpty ""

drawGigEntry : Gig -> H.Html
drawGigEntry gig =
  H.li []
    [ H.a [HA.href "#", HE.onClick (Signal.send actionChannel (ClickGig gig))]
      [ H.text gig.title
      , H.span [HA.class "badge"] [H.text "2 freie Plätze"]
      ]
    ]

view : Model -> H.Html
view model =
  case model.page of
    GigIndex ->
      H.div [HA.class "container"]
        [ H.div [HA.class "row"]
          [ H.div [HA.class "col-md-12"]
            [ H.h1 [] [H.text "Aufführungen"]
            , H.ul [HA.class "nav nav-pills nav-stacked"] (L.map drawGigEntry model.gigs)
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
            [ drawStand model.stand
            , H.text <| M.selectionsAsText model.stand
            ]
          ]
        ]


main : Signal H.Html
main =
  Signal.map view model

input : Signal Action
input = Signal.mergeMany [(Signal.map responseToAction2 gigsReceived), (Signal.map responseToAction seatsReceived), (Signal.subscribe actionChannel)]

updatesWithEffect : Signal (Model, Maybe Effect)
updatesWithEffect =
  Signal.foldp update (initialModel, Nothing) input

model : Signal Model
model =
  Signal.map fst updatesWithEffect

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel NoOp


-- SVG FOO

seatColor : M.Model -> M.Seat -> String
seatColor model seat =
  if | M.isSelected model seat -> "#f00"
     | M.isReserved model seat -> "#0f0"
     | otherwise -> "rgba(0,0,0,0)"

drawSeat : M.Model -> Int -> M.Seat -> S.Svg
drawSeat model rowNumber seat =
  let text = if M.isUsable seat then toString seat.number else ""
  in
    S.g [HE.onClick (Signal.send actionChannel (ClickSeat seat)), SA.transform <| "translate(" ++ (toString <| seat.x * 18) ++ ", 0)"]
      [ S.rect [SA.cursor "pointer", SA.style <| "fill:" ++ (seatColor model seat) ++ "; stroke-width: 1; stroke: rgb(0, 0, 0)", SA.width "18", SA.height "18"] []
      , S.text [SA.x "9", SA.y "13", SA.cursor "pointer", SA.textAnchor "middle", SA.fontSize "11"] [ H.text text]
      ]

drawRow : M.Model -> M.Row -> S.Svg
drawRow model row = S.g [SA.transform <| "translate(0," ++ (toString <| row.y * 25) ++ ")"]
  [ S.g [SA.transform "translate(20, 0)"] (L.map (drawSeat model row.number) (M.seatsInRow model row))
  , S.text [SA.x "5", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  , S.text [SA.x "1023", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  ]


drawStand : M.Model -> H.Html
drawStand model = S.svg [SA.version "1.1", SA.x "0", SA.y "0", SA.height "480", SA.width "1050"]
  [ S.g [SA.transform "translate(18, 1)"]
    (L.map (drawRow model) (M.rows model))
  ]
