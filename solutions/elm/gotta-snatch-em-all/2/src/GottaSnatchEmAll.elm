module GottaSnatchEmAll exposing (..)

import Set exposing (Set)


type alias Card =
    String


newCollection : Card -> Set Card
newCollection card =
    Set.singleton card


addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    let
        isMember = Set.member card collection
        newColl = Set.insert card collection
    in (isMember, newColl)


tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard yourCard theirCard collection =
    ( (Set.member yourCard collection) && not (Set.member theirCard collection)
    , collection
        |> Set.insert theirCard 
        |> Set.remove yourCard
    )




removeDuplicates : List Card -> List Card
removeDuplicates = Set.fromList >> Set.toList


extraCards : Set Card -> Set Card -> Int
extraCards yourCollection theirCollection =
    Set.diff yourCollection theirCollection
    |> Set.size

unionList: List (Set Card) -> Set Card
unionList = List.foldl Set.union Set.empty

boringCards : List (Set Card) -> List Card
boringCards collections =
    List.foldl
        Set.intersect
        (unionList collections)
        collections
    |> Set.toList


totalCards : List (Set Card) -> Int
totalCards =
    unionList >> Set.size


splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards collection =

    collection
    |> Set.partition (String.startsWith "Shiny")
    |> (\(shiny, notShiny) ->
        ((Set.toList shiny), (Set.toList notShiny)))
