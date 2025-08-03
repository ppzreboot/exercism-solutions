module GottaSnatchEmAll exposing (..)

import Set exposing (Set)


type alias Card =
    String


newCollection : Card -> Set Card
newCollection = Set.singleton


addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    ( Set.member card collection
    , Set.insert card collection
    )


tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard yourCard theirCard collection =
    let
        validAndValuable = (Set.member yourCard collection)
            && not (Set.member theirCard collection)
        after = collection
            |> Set.insert theirCard 
            |> Set.remove yourCard
    in (validAndValuable, after)




removeDuplicates : List Card -> List Card
removeDuplicates = Set.fromList >> Set.toList


extraCards : Set Card -> Set Card -> Int
extraCards otherColl= Set.diff otherColl >> Set.size

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
splitShinyCards =
    Set.partition (String.startsWith "Shiny")
    >> Tuple.mapBoth Set.toList Set.toList
