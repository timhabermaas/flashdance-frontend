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
import Maybe
import Time
import String

type alias Model = { title : String, counter : Int, seats : Stand }

type Action = NoOp | CountUp | CountDown | ClickSeat Seat | SetTitle String
type Effect = AjaxRequest String

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

row : Int -> List Seat -> List Seat
row n = List.map (\s -> { s | row <- n, pos <- (fst s.pos, (snd s.pos) + n) })

buildSeat : Int -> Seat
buildSeat n = {pos = (n,0), row = 16, number = n, selected = Unselected, state = Free}

dummyStand : Stand
dummyStand = List.foldl (\c acc -> acc ++ (row c <| List.map buildSeat [0..19])) [] [1..18]

view : Model -> Html
view model =
  div [ class "container" ]
    [ a [ class "btn btn-primary btn-small", href "#", onClick (Signal.send actionChannel CountUp) ] [ text "+" ],
      a [ class "btn btn-primary btn-small", href "#", onClick (Signal.send actionChannel CountDown) ] [ text "-" ],
      text (toString model.counter),
      text model.title,
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
drawStand stand = collage 1000 400 <| List.map drawSeat stand

main : Signal Html
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
