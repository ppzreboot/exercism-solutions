module Allergies exposing (Allergy(..), isAllergicTo, toList)


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    List.member allergy (toList score)


toList : Int -> List Allergy
toList score =
    let
        recurse : Int -> List Allergy -> List Allergy
        recurse restScore list =
            if restScore > 255 then
                recurse (restScore - 256) list
            else if restScore >= 128 then
                recurse (restScore - 128) (Cats :: list)
            else if restScore >= 64 then
                recurse (restScore - 64) (Pollen :: list)
            else if restScore >= 32 then
                recurse (restScore - 32) (Chocolate :: list)
            else if restScore >= 16 then
                recurse (restScore - 16) (Tomatoes :: list)
            else if restScore >= 8 then
                recurse (restScore - 8) (Strawberries :: list)
            else if restScore >= 4 then
                recurse (restScore - 4) (Shellfish :: list)
            else if restScore >= 2 then
                recurse (restScore - 2) (Peanuts :: list)
            else if restScore == 1 then
                Eggs :: list
            else
                list
    in
        recurse score []
