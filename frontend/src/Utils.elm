module Utils exposing (..)

import Html exposing (Html, text)

isJust : Maybe a -> Bool
isJust m = case m of
             Just _ -> True
             Nothing -> False

isNothing : Maybe a -> Bool
isNothing m = case m of
                Just _ -> False
                Nothing -> True

curry : ((a, b) -> c) -> a -> b -> c
curry f a b = f (a,b)

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

when : Bool -> Html a -> Html a
when p m = if p then m else empty

unless : Bool -> Html a -> Html a
unless = when << not

empty : Html a
empty = text ""
