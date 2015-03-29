module Flashdance where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal
import List
import Text
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Collage (..)
import Color (..)

type alias Model = { counter : Int, seats : Stand }

type Action = NoOp | CountUp | CountDown | ClickSeat Seat

type SeatState = Reserved | Unusable | Free
type Selected = Selected | Unselected

type alias Seat = { pos: (Int,Int), row: Int, number: Int, selected: Selected, state: SeatState }
type alias Stand = List Seat


toggleSelected : Seat -> Seat
toggleSelected s = case s.selected of
  Selected -> { s | selected <- Unselected }
  Unselected -> { s | selected <- Selected }

updateStand : Action -> Stand -> Stand
updateStand action stand =
  case action of
    ClickSeat seat -> List.map (\s -> if s == seat then toggleSelected s else s) stand
    _ -> stand

update : Action -> Model -> Model
update action model =
  case action of
    CountUp -> { model | counter <- model.counter + 1 }
    CountDown -> { model | counter <- model.counter - 1 }
    ClickSeat seat -> { model | seats <- updateStand (ClickSeat seat) model.seats }
    NoOp -> model

dummyStand : Stand
dummyStand = [{pos = (0,0), row = 14, number = 1, selected = Unselected, state = Free}, {pos = (1,0), row = 14, number = 2, selected = Unselected, state = Free}]

view : Model -> Html
view model =
  div [ class "container" ]
    [ a [ class "btn btn-primary btn-small", href "#", onClick (Signal.send actionChannel CountUp) ] [ text "+" ],
      a [ class "btn btn-primary btn-small", href "#", onClick (Signal.send actionChannel CountDown) ] [ text "-" ],
      text (toString model.counter),
      div [] [
        fromElement (drawStand model.seats)
      ]
    ]

scaleTuple : (Int,Int) -> Float -> (Float,Float)
scaleTuple (x,y) s = ((toFloat x) * s, (toFloat y) * s)


seatShape : Seat -> (Shape -> Form)
seatShape s = case s.state of
  Reserved -> outlined (solid black)
  Unusable -> outlined (solid black)
  Free -> if (s.selected == Selected) then outlined (solid red) else outlined (solid black)

drawSeat : Seat -> Form
drawSeat seat = move (scaleTuple seat.pos 21) <|
                toForm <|
                clickable (Signal.send actionChannel (ClickSeat seat)) <|
                collage 20 20
                  [ group
                    [ (seatShape seat) (rect 20.0 20.0),
                      toForm <| Text.centered <| Text.fromString <| toString seat.number
                    ]
                  ]


drawStand : Stand -> Element
drawStand stand = collage 400 400 <| List.map drawSeat stand

main : Signal Html
main =
  Signal.map view model

model : Signal Model
model =
  Signal.foldp update {counter = 0, seats = dummyStand} (Signal.subscribe actionChannel)

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel CountUp
