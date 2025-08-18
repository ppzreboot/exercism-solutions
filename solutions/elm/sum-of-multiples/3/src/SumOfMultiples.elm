module SumOfMultiples exposing (sumOfMultiples)
import Set exposing (..)

sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    let
        getMultiples : Int -> List Int
        getMultiples base =
            List.range 1 ((limit - 1) // base)
            |> List.map ((*) base)
        
        allMultiples : List Int -> Set Int -> Set Int
        allMultiples restDivisors lastResult =
            case restDivisors of
                [] -> lastResult
                divisor :: nextRest ->
                    divisor
                    |> getMultiples
                    |> Set.fromList
                    |> Set.union lastResult
                    |> allMultiples nextRest
        
        all = allMultiples divisors Set.empty
    in
        Set.foldl
            (\multiple sum -> multiple + sum)
            0
            all
