module EliudsEggs exposing (eggCount)
import Bitwise


eggCount : Int -> Int
eggCount n =
    List.range 0 30
    |> List.map (\index -> Bitwise.and (2 ^ index) n)
    |> List.filter (\r -> r > 0)
    |> List.length
