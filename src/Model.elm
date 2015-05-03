module Model (Model, Seat, Row, Reservation, selectionsAsText, isUsable, updateSeats, updateReservations, reserveSeats, rows, seatsInRow, initialModel, isSelected, isReserved, toggleSelected, clearSelections) where
import Dict as D
import List as L
import String as S

type alias SeatId = String
type alias Reservation = { seatId: String }


type alias Row = { number: Int, y: Int }
type alias Seat = { id: SeatId, x: Int, number: Int, row: Int, usable: Bool }

type alias Model = { reservations: List Reservation, seats: List Seat, rows: List Row, selections: List Seat }


toggleSelected : Model -> Seat -> Model
toggleSelected model seat =
  case seat.usable of
    True -> if | isSelected model seat -> { model | selections <- L.filter ((/=) seat) model.selections }
               | isReserved model seat -> model
               | otherwise -> { model | selections <- seat :: model.selections }
    False -> model

initialModel : Model
initialModel = {selections = [], rows = [], seats = [], reservations = []}

isSelected : Model -> Seat -> Bool
isSelected m s = L.member s m.selections

isUsable : Seat -> Bool
isUsable = .usable

isReserved : Model -> Seat -> Bool
isReserved m s = L.member s.id <| L.map .seatId m.reservations


rows : Model -> List Row
rows model = model.rows

seatsInRow : Model -> Row -> List Seat
seatsInRow model row = L.filter (\s -> s.row == row.number) model.seats

updateSeats : Model -> List Seat -> List Row -> Model
updateSeats model seats rows = { model | rows <- rows, seats <- seats }

updateReservations : Model -> List Reservation -> Model
updateReservations model reservations = { model | reservations <- reservations }

reserveSeats : Model -> List Seat -> Model
reserveSeats model seats =
  let newReservations = L.map (\s -> {seatId = s.id}) seats
  in
    { model | reservations <- model.reservations ++ newReservations }

selectionsAsText : List Seat -> String
selectionsAsText selections = S.concat <| L.intersperse ", " <| L.map (\s -> "(" ++ toString s.row ++ ", " ++ toString s.number ++ ")") selections

clearSelections : Model -> Model
clearSelections m = { m | selections <- [] }
