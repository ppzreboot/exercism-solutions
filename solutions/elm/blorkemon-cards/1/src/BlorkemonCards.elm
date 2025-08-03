module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )


type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power

maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power

sortByMonsterName : List Card -> List Card
sortByMonsterName cards =
    List.sortBy (\a -> a.power) cards


sortByCoolness : List Card -> List Card
sortByCoolness cards =
    List.sortWith
        (\a b ->
            case (a.shiny, b.shiny) of
                (True, False) -> GT
                (False, True) -> LT
                _ ->
                    case compare a.power b.power of
                        GT -> GT
                        LT -> LT
                        EQ -> EQ
        )
        cards

compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    case compare card1.power card2.power of
        GT -> GT
        LT -> LT
        EQ ->
            case (card1.shiny, card2.shiny) of
                (True, False) -> GT
                (False, True) -> LT
                _ -> EQ


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        EQ -> "too close to call"
        GT -> card1.monster
        LT -> card2.monster
