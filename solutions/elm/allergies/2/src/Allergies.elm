module Allergies exposing (Allergy(..), isAllergicTo, toList)
import Bitwise
import Array

type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

allAllergies : List Allergy
allAllergies =
    [ Eggs
    , Peanuts
    , Shellfish
    , Strawberries
    , Tomatoes
    , Chocolate
    , Pollen
    , Cats
    ]

findIndex : a -> List a -> Int
findIndex element list =
    let
        recurse : Int -> a -> List a -> Int
        recurse index el lst =
            case lst of
                [] -> index
                head :: rest ->
                    if head == el then
                        index
                    else
                        recurse (index + 1) el rest
    in
        recurse 0 element list

isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    findIndex allergy allAllergies
    |> (\index -> 
        (Bitwise.and score (2 ^ index)) > 0
    )

toList : Int -> List Allergy
toList score =
    List.filter (\a -> isAllergicTo a score) allAllergies
