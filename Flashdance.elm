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

type alias Model = { title : String, counter : Int, seats : Stand }

type Action = NoOp | CountUp | CountDown | ClickSeat Seat | SetTitle String
type Effect = AjaxRequest String

type SeatState = Reserved | Unusable | Free
type Selected = Selected | Unselected

type alias Seat = { x: Int, number: Int, selected: Selected, state: SeatState }
type alias Row = { number: Int, y: Int, seats: List Seat }
type alias Stand = List Row


toggleSelected : Seat -> Seat
toggleSelected s = case s.selected of
  Selected -> { s | selected <- Unselected }
  Unselected -> { s | selected <- Selected }

updateStand : Action -> Stand -> Stand
updateStand action stand =
  case action of
    ClickSeat seat -> stand
    _ -> stand

update : Action -> (Model, Maybe Effect) -> (Model, Maybe Effect)
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

dummyRow : List Seat
dummyRow = List.map (\n -> {x = n, number = n, selected = Unselected, state = Free}) [1..100]

dummyStand : Stand
dummyStand = List.map (\n -> { number = 17 - n, y = n, seats = dummyRow }) [1..30]

view : Model -> H.Html
view model =
    H.div []
      [ drawStand model.seats
      , H.text <| toString model.counter
      , H.button [ HE.onClick (Signal.send actionChannel CountUp) ] [H.text "fo"]
      ]

scaleTuple : (Int,Int) -> Float -> (Float,Float)
scaleTuple (x,y) s = ((toFloat x) * s, (toFloat y) * s)


seatShape : Seat -> (Shape -> Form)
seatShape s = case s.state of
  Reserved -> outlined (solid black)
  Unusable -> outlined (solid black)
  Free -> if (s.selected == Selected) then outlined (solid red) else outlined (solid black)


main : Signal H.Html
main =
  Signal.map view model

input : Signal Action
input = Signal.merge (Signal.map responseToAction asyncResponses) (Signal.subscribe actionChannel)

updatesWithEffect : Signal (Model, Maybe Effect)
updatesWithEffect =
  Signal.foldp update ({title = "", counter = 0, seats = dummyStand}, Nothing) input


toModel : (Model, Maybe Effect) -> Model
toModel (m, _) = m

model : Signal Model
model =
  Signal.map toModel updatesWithEffect

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel CountUp


-- SVG FOO

drawSeat : Seat -> S.Svg
drawSeat seat = S.g [HE.onClick (Signal.send actionChannel CountUp), SA.transform <| "translate(" ++ (toString <| seat.x * 18) ++ ", 0)"]
                  [ S.rect [SA.style "fill:rgba(0,0,0,0);stroke-width: 1; stroke: rgb(0, 0, 0)", SA.width "18", SA.height "18"] []
                  , S.text [SA.x "9", SA.y "14", SA.textAnchor "middle"] [ H.text <| toString seat.number]
                  ]

drawRow : Row -> S.Svg
drawRow row = S.g [SA.transform <| "translate(0," ++ (toString <| row.y * 18) ++ ")"] (List.map drawSeat row.seats)

drawStand : Stand -> H.Html
drawStand stand = S.svg [SA.version "1.1", SA.x "0", SA.y "0", SA.height "500", SA.width "1000"] <| List.map drawRow stand
