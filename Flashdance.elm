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

type Action = NoOp | CountUp | CountDown | ClickSeat M.Seat | SetTitle String
type Effect = AjaxRequest String




update : Action -> (M.Model, Maybe Effect) -> (M.Model, Maybe Effect)
update action (model, _) =
  case action of
    CountUp -> ({ model | counter <- model.counter + 1 }, Nothing)
    CountDown -> ({ model | counter <- model.counter - 1 }, Just <| AjaxRequest <| toString (model.counter - 1))
    ClickSeat seat -> (M.toggleSelected model seat, Nothing)
    SetTitle title -> ({ model | title <- title }, Nothing)
    NoOp -> (model, Nothing)

port requests : Signal String
port requests = fooRequest

type alias GithubResponse = { title : String }

port asyncResponses : Signal GithubResponse

responseToAction : GithubResponse -> Action
responseToAction r = SetTitle r.title


toRequest : Maybe Effect -> String
toRequest e = case e of
  (Just (AjaxRequest s)) -> s
  Nothing -> ""

fooRequest : Signal String
fooRequest = Signal.map (snd >> toRequest) updatesWithEffect |> Signal.dropIf String.isEmpty ""


view : M.Model -> H.Html
view model =
    H.div []
      [ drawStand model
      , H.text <| toString model.counter
      , H.button [ HE.onClick (Signal.send actionChannel CountDown) ] [H.text "fo"]
      ]


main : Signal H.Html
main =
  Signal.map view model

input : Signal Action
input = Signal.merge (Signal.map responseToAction asyncResponses) (Signal.subscribe actionChannel)

updatesWithEffect : Signal (M.Model, Maybe Effect)
updatesWithEffect =
  Signal.foldp update (M.initialModel, Nothing) input


justModel : (M.Model, Maybe Effect) -> M.Model
justModel (m, _) = m

model : Signal M.Model
model =
  Signal.map justModel updatesWithEffect

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel CountUp


-- SVG FOO

seatColor : M.Model -> M.Seat -> String
seatColor model seat =
  if M.isSelected model seat then "#f00" else "rgba(0,0,0,0)"

drawSeat : M.Model -> Int -> M.Seat -> S.Svg
drawSeat model rowNumber seat = S.g [HE.onClick (Signal.send actionChannel (ClickSeat seat)), SA.transform <| "translate(" ++ (toString <| seat.x * 18) ++ ", 0)"]
                  [ S.rect [SA.cursor "pointer", SA.style <| "fill:" ++ (seatColor model seat) ++ "; stroke-width: 1; stroke: rgb(0, 0, 0)", SA.width "18", SA.height "18"] []
                  , S.text [SA.x "9", SA.y "13", SA.cursor "pointer", SA.textAnchor "middle", SA.fontSize "11"] [ H.text <| toString seat.number]
                  ]

drawRow : M.Model -> M.Row -> S.Svg
drawRow model row = S.g [SA.transform <| "translate(0," ++ (toString <| (row.y - 1) * 25) ++ ")"]
  [ S.g [SA.transform "translate(20, 0)"] (L.map (drawSeat model row.number) (M.seatsInRow model row))
  , S.text [SA.x "5", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  , S.text [SA.x "1023", SA.y "14", SA.textAnchor "end"] [ H.text <| toString row.number]
  ]


drawStand : M.Model -> H.Html
drawStand model = S.svg [SA.version "1.1", SA.x "0", SA.y "0", SA.height "480", SA.width "1050"]
  [ S.g [SA.transform "translate(18, 1)"]
    (L.map (drawRow model) (M.rows model))
  ]
