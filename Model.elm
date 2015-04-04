module Model (Model, Stand, Seat, Row, isSelected, toggleSelected, dummyStand) where
import Dict as D
import List as L

type SeatState = Reserved | Unusable | Free
type Selected = Selected | Unselected

type alias Row = { number: Int, y: Int, seats: List Seat }
type alias Stand = D.Dict Int Row
type alias Seat = { x: Int, number: Int, selected: Selected, state: SeatState }

type alias Model = { title : String, counter : Int, seats : Stand }

toggleSelected : Seat -> Seat
toggleSelected s = case s.selected of
  Selected -> { s | selected <- Unselected }
  Unselected -> { s | selected <- Selected }

dummyRow : List Seat
dummyRow = L.map (\n -> {x = n, number = n, selected = Unselected, state = Free}) [1..40]

dummyStand : Stand
dummyStand = L.foldl (\n map -> D.insert n {number = 17 - n, y = n, seats = dummyRow } map) D.empty [1..100]

isSelected : Seat -> Bool
isSelected s = case s.selected of
  Selected -> True
  Unselected -> False

--selectSeat : Model -> Seat -> Model
