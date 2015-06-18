module HttpRequests (fetchGigs, fetchSeats, fetchReservations, submitOrder, startOrder, reserveSeat, freeSeat, finishOrder, finishOrderWithAddress, login, orders, paid, unpaid, cancelOrder) where
import Model as M
import Http
import Date
import Dict
import Task exposing (..)
import Json.Decode exposing (..)
import Json.Encode as JE

baseApiEndpoint : String
baseApiEndpoint = "https://tickets-backend-ruby.herokuapp.com"

-- TODO this is duplicated in Flashdance.elm
type alias GigId = String
type alias OrderId = String
type alias SeatId = String
type alias Address' = { street: String, postalCode: String, city: String }
type alias Gig = { id: GigId, date: Date.Date, title: String, freeSeats: Int }
type alias Order = { id: OrderId, createdAt: Date.Date, name: String, email: String, paid: Bool, reducedCount: Int, seatIds: List SeatId, number: Int, address: Maybe Address'}

gigDecoder : Decoder (List Gig)
gigDecoder =
  Json.Decode.list (Json.Decode.object4 Gig ("id" := Json.Decode.string) ("date" := dateDecoder) ("title" := Json.Decode.string) ("freeSeats" := Json.Decode.int))

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

dateDecoder : Decoder Date.Date
dateDecoder = Json.Decode.customDecoder Json.Decode.string Date.fromString

fetchGigs : Task Http.Error (List Gig)
fetchGigs =
  get gigDecoder (baseApiEndpoint ++ "/gigs")

fetchSeats : GigId -> Task Http.Error (List M.Seat, List M.Row)
fetchSeats id =
  get seatsDecoder (baseApiEndpoint ++ "/gigs/" ++ id ++ "/seats")

fetchReservations : GigId -> Task Http.Error (List M.Reservation)
fetchReservations id =
  get reservationsDecoder (baseApiEndpoint ++ "/gigs/" ++ id ++ "/reservations")


orderEncoder : String -> String -> (List String) -> Int -> String
orderEncoder name email seatIds reducedCount = JE.encode 0 (JE.object [("name", JE.string name), ("reducedCount", JE.int reducedCount), ("email", JE.string email), ("seatIds", JE.list (List.map JE.string seatIds))])

submitOrder : String -> String -> String -> List String -> Int -> Task Http.Error String
submitOrder gigId name email seatIds reducedCount =
  post (Json.Decode.succeed "") (baseApiEndpoint ++ "/gigs/" ++ gigId ++ "/orders") (Http.string (orderEncoder name email seatIds reducedCount))

startOrder : String -> String -> Task Http.Error String
startOrder name email =
  post (Json.Decode.at ["orderId"] Json.Decode.string) (baseApiEndpoint ++ "/orders") (Http.string (orderEncoder name email [] 0))

finishOrderEncoder : Int -> String -> String
finishOrderEncoder reducedCount type' = JE.encode 0 (JE.object [("reducedCount", JE.int reducedCount), ("type", JE.string type')])

finishOrder : String -> Int -> String -> Task Http.Error String
finishOrder orderId reducedCount type' =
  put (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId ++ "/finish") (Http.string <| finishOrderEncoder reducedCount type')

finishOrderWithAddressEncoder : Int -> String -> String -> String -> String
finishOrderWithAddressEncoder reducedCount street postalCode city = JE.encode 0 (JE.object [("reducedCount", JE.int reducedCount), ("address", JE.object [("street", JE.string street), ("postalCode", JE.string postalCode), ("city", JE.string city)])])

finishOrderWithAddress : String -> Int -> String -> String -> String -> Task Http.Error String
finishOrderWithAddress orderId reducedCount street postalCode city =
  put (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId ++ "/finish") (Http.string <| finishOrderWithAddressEncoder reducedCount street postalCode city)

requestWithCredentials : String -> String -> String -> Json.Decode.Decoder value -> String -> Http.Body -> Task Http.Error value
requestWithCredentials user pw verb decoder url body =
  let headers = if user /= "" then [("X-User", user), ("X-Password", pw)] else []
      request =
        { verb = verb
        , headers = headers
        , url = url
        , body = body
        }
  in
      Http.fromJson decoder (Http.send Http.defaultSettings request)

addressDecoder : Decoder Address'
addressDecoder = Json.Decode.object3 Address' ("street" := Json.Decode.string) ("postalCode" := Json.Decode.string) ("city" := Json.Decode.string)

ordersDecoder : Decoder (List Order)
ordersDecoder = Json.Decode.list orderDecoder

unwrap : Maybe a -> a
unwrap x =
  case x of
    Just j -> j

unwrapResult : Result x y -> y
unwrapResult r =
  case r of
    Ok r -> r

orderDecoder : Decoder Order
orderDecoder =
  let foo key decoder v = unwrapResult <| Json.Decode.decodeValue decoder (unwrap <| Dict.get key v)
  in
      Json.Decode.customDecoder (Json.Decode.dict Json.Decode.value) (\v ->
        Ok { id = foo "id" string v
           , createdAt = foo "createdAt" dateDecoder v
           , name = foo "name" string v
           , email = foo "email" string v
           , paid = foo "paid" bool v
           , reducedCount = foo "reducedCount" int v
           , seatIds = foo "seatIds" (list string) v
           , number = foo "number" int v
           , address = case Dict.get "address" v of
             Just a ->
               case decodeValue addressDecoder a of
                 Ok f -> Just f
                 Err _ -> Nothing
             Nothing -> Nothing
        }
      )


orders : Task Http.Error (List Order)
orders = get ordersDecoder (baseApiEndpoint ++ "/orders")

post    = requestWithCredentials "" "" "POST"
put     = requestWithCredentials "" "" "PUT"
delete  = requestWithCredentials "" "" "DELETE"
get decoder url = requestWithCredentials "" "" "GET" decoder url (Http.empty)

paid : OrderId -> String -> String -> Task Http.Error String
paid orderId name password =
  put (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId ++ "/pay") (Http.empty)

unpaid : OrderId -> String -> String -> Task Http.Error String
unpaid orderId name password =
  put (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId ++ "/unpay") (Http.empty)


reserveSeat : OrderId -> SeatId -> Task Http.Error String
reserveSeat orderId seatId =
  put (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId ++ "/reservations/" ++ seatId) (Http.empty)

freeSeat : OrderId -> SeatId -> Task Http.Error String
freeSeat orderId seatId =
  delete (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId ++ "/reservations/" ++ seatId) (Http.empty)

login : String -> String -> Task Http.Error String
login user password =
  post (Json.Decode.at ["role"] Json.Decode.string) (baseApiEndpoint ++ "/login") (Http.string (JE.encode 0 (JE.object [("user", JE.string user), ("password", JE.string password)])))

cancelOrder : OrderId -> Task Http.Error String
cancelOrder orderId =
  delete (Json.Decode.succeed "") (baseApiEndpoint ++ "/orders/" ++ orderId) (Http.empty)
