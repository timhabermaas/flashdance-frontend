module HttpRequests (fetchGigs, fetchSeats, fetchReservations, submitOrder) where
import Model as M
import Http
import Task exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)

baseApiEndpoint : String
baseApiEndpoint = "https://tickets-backend-ruby.herokuapp.com"

-- TODO this is duplicated in Flashdance.elm
type alias GigId = String
type alias Gig = { id: GigId, date: String, title: String }

gigDecoder : Decoder (List Gig)
gigDecoder =
  Json.Decode.list (object3 Gig ("id" := Json.Decode.string) ("date" := Json.Decode.string) ("title" := Json.Decode.string))

reservationDecoder : Decoder M.Reservation
reservationDecoder =
  object1 M.Reservation ("seatId" := Json.Decode.string)

reservationsDecoder : Decoder (List M.Reservation)
reservationsDecoder = Json.Decode.list reservationDecoder

seatDecoder : Decoder M.Seat
seatDecoder =
  object5 M.Seat
    ("id" := Json.Decode.string)
    ("x" := Json.Decode.int)
    ("number" := Json.Decode.int)
    ("row" := Json.Decode.int)
    ("usable" := Json.Decode.bool)

rowDecoder : Decoder M.Row
rowDecoder =
  object2 M.Row
    ("number" := Json.Decode.int)
    ("y" := Json.Decode.int)

seatsDecoder : Decoder (List M.Seat, List M.Row)
seatsDecoder =
  object2 (,)
    ("seats" := (Json.Decode.list seatDecoder))
    ("rows" := (Json.Decode.list rowDecoder))

fetchGigs : Task Http.Error (List Gig)
fetchGigs =
  Http.get gigDecoder (baseApiEndpoint ++ "/gigs")

fetchSeats : GigId -> Task Http.Error (List M.Seat, List M.Row)
fetchSeats id =
  Http.get seatsDecoder (baseApiEndpoint ++ "/gigs/" ++ id ++ "/seats")

fetchReservations : GigId -> Task Http.Error (List M.Reservation)
fetchReservations id =
  Http.get reservationsDecoder (baseApiEndpoint ++ "/gigs/" ++ id ++ "/reservations")


orderEncoder : String -> String -> (List String) -> String
orderEncoder name email seatIds = Json.Encode.encode 0 (object [("name", Json.Encode.string name), ("email", Json.Encode.string email), ("seatIds", Json.Encode.list (List.map Json.Encode.string seatIds))])

submitOrder : String -> String -> String -> List String -> Task Http.Error String
submitOrder gigId name email seatIds =
  Http.post Json.Decode.string (baseApiEndpoint ++ "/gigs/" ++ gigId ++ "/orders") (Http.string (orderEncoder name email seatIds))
