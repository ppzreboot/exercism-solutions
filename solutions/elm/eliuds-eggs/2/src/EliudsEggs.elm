module EliudsEggs exposing (eggCount)

eggCount : Int -> Int
eggCount n =
    case n of
        0 -> 0
        1 -> 1
        _ ->
            (modBy 2 n) + (eggCount (n // 2))
