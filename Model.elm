module Model (Model, Stand, Seat, Row, selectionsAsText, isUsable, updateSeats, rows, seatsInRow, initialModel, isSelected, toggleSelected) where
import Dict as D
import List as L
import String as S

type SeatState = Reserved | Unusable | Free

type alias SeatId = Int


type alias Row = { number: Int, y: Int }
type alias Stand = D.Dict Int Row
type alias Seat = { id : SeatId, x: Int, number: Int, row: Int, usable: Bool }

type alias Model = { title : String, counter : Int, seats : List Seat, rows: List Row, selections : List Seat }


toggleSelected : Model -> Seat -> Model
toggleSelected model seat =
  case seat.usable of
    True -> if | isSelected model seat -> { model | selections <- L.filter ((/=) seat) model.selections }
               | otherwise -> { model | selections <- seat :: model.selections }
    False -> model

dummyRow : Int -> List Seat
dummyRow row = L.map (\n -> {id = row * n, x = n, number = n, row = row, usable = True}) [1..48]

dummyStand : List Seat
dummyStand = L.concat <| L.map dummyRow [1..16]

dummyRows : List Row
dummyRows = L.map (\n -> {number = 17 - n, y = n}) [1..16]

initialModel : Model
initialModel = {selections = [], title = "", counter = 0, rows = dummyRows, seats = dummyStand}

isSelected : Model -> Seat -> Bool
isSelected m s = L.member s m.selections

isUsable : Seat -> Bool
isUsable = .usable


rows : Model -> List Row
rows model = model.rows

seatsInRow : Model -> Row -> List Seat
seatsInRow model row = L.filter (\s -> s.row == row.number) model.seats

updateSeats : Model -> List Seat -> Model
updateSeats model seats = { model | seats <- seats }

selectionsAsText : Model -> String
selectionsAsText model = S.concat <| L.intersperse ", " <| L.map (\s -> "(" ++ toString s.row ++ ", " ++ toString s.number ++ ")") model.selections
