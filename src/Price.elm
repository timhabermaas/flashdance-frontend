module Price (Price, format, fromInt, add) where

type Price = EUR Int

add : Price -> Price -> Price
add (EUR x) (EUR y) = EUR <| x + y

fromInt : Int -> Price
fromInt x = EUR x

format : Price -> String
format p =
  case p of
    EUR x -> "€ " ++ toString ((toFloat x) / 100)
