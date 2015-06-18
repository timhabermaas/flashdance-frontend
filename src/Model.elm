module Model (Model, Seat, Row, Reservation, selectionsAsText, isUsable, updateSeats, updateReservations, reserveSeats, freeSeats, rows, seatsInRow, initialModel, isSelected, isReserved, selectSeat, unselectSeat, clearSelections, findSeat, selectSeatIds) where
import Dict as D
import List as L
import String as S

type alias SeatId = String
type alias Reservation = { seatId: SeatId }


type alias Row = { number: Int, y: Int }
type alias Seat = { id: SeatId, x: Int, number: Int, row: Int, usable: Bool }

type alias Model = { reservations: List Reservation, seats: List Seat, rows: List Row, selections: List Seat }

selectSeat : Model -> Seat -> Model
selectSeat model seat =
  case seat.usable of
    True -> if | isReserved model seat -> model
               | otherwise -> alwaysSelectSeat model seat
    False -> model

alwaysSelectSeat : Model -> Seat -> Model
alwaysSelectSeat model seat =
  { model | selections <- seat :: model.selections }

unwrap : Maybe x -> x
unwrap m =
  case m of
    Maybe.Just j -> j

freeSeats : Model -> List SeatId -> Model
freeSeats model seatIds =
  {model | reservations <- List.filter (\r -> not <| List.member r.seatId seatIds) model.reservations}


compact : List (Maybe a) -> List a
compact = List.map unwrap << List.filter (\e -> case e of
                                                  Just _ -> True
                                                  Nothing -> False
                                         )

selectSeatIds : Model -> List SeatId -> Model
selectSeatIds model seatIds =
  let selectSeats model seats = List.foldl (\s m -> alwaysSelectSeat m s) model seats
  in  selectSeats model (compact <| List.map (\s -> findSeat model s) seatIds)


unselectSeat : Model -> Seat -> Model
unselectSeat model seat =
  case seat.usable of
    True -> if | isReserved model seat -> model
               | otherwise -> { model | selections <- L.filter ((/=) seat) model.selections }
    False -> model

initialModel : Model
initialModel = {selections = [], rows = [], seats = [], reservations = []}

isSelected : Model -> Seat -> Bool
isSelected m s = L.member s m.selections

isUsable : Seat -> Bool
isUsable = .usable

isReserved : Model -> Seat -> Bool
isReserved m s = L.member s.id <| L.map .seatId m.reservations


findSeat : Model -> SeatId -> Maybe Seat
findSeat model seatId =
  case (L.filter (\s -> s.id == seatId) model.seats) of
    [] -> Nothing
    (x::xs) -> Just x

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
