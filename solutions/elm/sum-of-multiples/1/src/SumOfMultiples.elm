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
        allMultiples restDivisors result =
            case restDivisors of
                [] -> result
                divisor :: nextRest ->
                    Set.union result (Set.fromList (getMultiples divisor))
                    |> allMultiples nextRest
        
        all = allMultiples divisors Set.empty
    in
        List.foldl
            (\multiple sum -> multiple + sum)
            0
            (Set.toList all)
