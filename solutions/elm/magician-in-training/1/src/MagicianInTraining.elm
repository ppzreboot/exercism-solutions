module MagicianInTraining exposing (..)

import Array exposing (Array)


getCard : Int -> Array Int -> Maybe Int
getCard index deck =
    Array.get index deck


setCard : Int -> Int -> Array Int -> Array Int
setCard index newCard deck =
    Array.set index newCard deck


addCard : Int -> Array Int -> Array Int
addCard newCard deck =
    Array.push newCard deck


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    let
        len = Array.length deck
    in
        if index > len - 1
        then deck
        else Array.append
            (Array.slice 0 index deck)
            (Array.slice (index + 1) len deck)


evenCardCount : Array Int -> Int
evenCardCount =
    Array.filter (\n -> modBy 2 n == 0)
    >> Array.length
