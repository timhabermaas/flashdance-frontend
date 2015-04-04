module Model (Model, Stand, Seat, Row, rows, seatsInRow, initialModel, isSelected, toggleSelected) where
import Dict as D
import List as L

type SeatState = Reserved | Unusable | Free

type alias RowNumber = Int
type alias SeatNumber = Int
type alias SeatId = (RowNumber, SeatNumber)


type alias Row = { number: Int, y: Int, seats: List Seat }
type alias Stand = D.Dict Int Row
type alias Seat = { seatId : SeatId, x: Int, number: Int, row: Int, state: SeatState }

type alias Model = { title : String, counter : Int, seats : Stand, selections : List Seat }


toggleSelected : Model -> Seat -> Model
toggleSelected model seat = if | isSelected model seat -> { model | selections <- L.filter ((/=) seat) model.selections }
                               | otherwise -> { model | selections <- seat :: model.selections }

dummyRow : Int -> List Seat
dummyRow row = L.map (\n -> {seatId = (row, n), x = n, number = n, row = row, state = Free}) [1..48]

dummyStand : Stand
dummyStand = L.foldl (\n map -> D.insert (16 - n) {number = 16 - n, y = n, seats = dummyRow n } map) D.empty [1..16]

initialModel : Model
initialModel = {selections = [], title = "", counter = 0, seats = dummyStand}

isSelected : Model -> Seat -> Bool
isSelected m s = L.member s m.selections

rows : Model -> List Row
rows model = D.values model.seats

seatsInRow : Model -> Row -> List Seat
seatsInRow model row =
  case D.get row.number model.seats of
    Nothing -> []
    Just row -> row.seats
