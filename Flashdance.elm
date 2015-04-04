module Flashdance where

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Signal
import List
import Text
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Collage (..)
import Color (..)
import Maybe
import Time
import String
import Dict as D
import Model as M

type Action = NoOp | CountUp | CountDown | ClickSeat (Int,Int) | SetTitle String
type Effect = AjaxRequest String



markSeatInSeats : Int -> List M.Seat -> List M.Seat
markSeatInSeats number seats = List.map (\s -> if s.number == number then M.toggleSelected s else s ) seats

updateStand : Action -> M.Stand -> M.Stand
updateStand action stand =
  case action of
    ClickSeat (row, number) -> D.update row (\row -> Maybe.andThen row (\row -> Just { row | seats <- markSeatInSeats number row.seats })) stand
    _ -> stand

update : Action -> (M.Model, Maybe Effect) -> (M.Model, Maybe Effect)
update action (model, _) =
  case action of
    CountUp -> ({ model | counter <- model.counter + 1 }, Nothing)
    CountDown -> ({ model | counter <- model.counter - 1 }, Just <| AjaxRequest <| toString (model.counter - 1))
    ClickSeat seat -> ({ model | seats <- updateStand (ClickSeat seat) model.seats }, Nothing)
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
      [ drawStand model.seats
      , H.text <| toString model.counter
      , H.button [ HE.onClick (Signal.send actionChannel CountUp) ] [H.text "fo"]
      ]

scaleTuple : (Int,Int) -> Float -> (Float,Float)
scaleTuple (x,y) s = ((toFloat x) * s, (toFloat y) * s)



main : Signal H.Html
main =
  Signal.map view model

input : Signal Action
input = Signal.merge (Signal.map responseToAction asyncResponses) (Signal.subscribe actionChannel)

updatesWithEffect : Signal (M.Model, Maybe Effect)
updatesWithEffect =
  Signal.foldp update ({title = "", counter = 0, seats = M.dummyStand}, Nothing) input


toModel : (M.Model, Maybe Effect) -> M.Model
toModel (m, _) = m

model : Signal M.Model
model =
  Signal.map toModel updatesWithEffect

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel CountUp


-- SVG FOO

seatColor : M.Seat -> String
seatColor s =
  if M.isSelected s then "#f00" else "rgba(0,0,0,0)"

drawSeat : Int -> M.Seat -> S.Svg
drawSeat rowNumber seat = S.g [HE.onClick (Signal.send actionChannel (ClickSeat (17 - rowNumber, seat.number))), SA.transform <| "translate(" ++ (toString <| seat.x * 18) ++ ", 0)"]
                  [ S.rect [SA.style <| "fill:" ++ (seatColor seat) ++ "; stroke-width: 1; stroke: rgb(0, 0, 0)", SA.width "18", SA.height "18"] []
                  , S.text [SA.x "9", SA.y "14", SA.textAnchor "middle"] [ H.text <| toString seat.number]
                  ]

drawRow : M.Row -> S.Svg
drawRow row = S.g [SA.transform <| "translate(0," ++ (toString <| row.y * 18) ++ ")"] (List.map (drawSeat row.number) row.seats)

drawStand : M.Stand -> H.Html
drawStand stand = S.svg [SA.version "1.1", SA.x "0", SA.y "0", SA.height "500", SA.width "1000"] <| List.map drawRow (D.values stand)
