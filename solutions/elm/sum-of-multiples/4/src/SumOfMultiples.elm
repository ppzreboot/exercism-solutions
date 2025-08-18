module SumOfMultiples exposing (sumOfMultiples)
import Set exposing (..)
import Html.Attributes exposing (multiple)

getMultiples : Int -> Int -> List Int
getMultiples limit base =
    let
        r : Int -> List Int -> List Int
        r index result =
            let multiple = base * index
            in
                if multiple >= limit then
                    result
                else
                    r (index + 1) (multiple :: result)
    in r 1 []

sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    let
        allMultiples : List Int -> Set Int -> Set Int
        allMultiples restDivisors lastResult =
            case restDivisors of
                [] -> lastResult
                divisor :: nextRest ->
                    divisor
                    |> getMultiples limit
                    |> Set.fromList
                    |> Set.union lastResult
                    |> allMultiples nextRest
        
        all = allMultiples divisors Set.empty
    in
        Set.foldl
            (\multiple sum -> multiple + sum)
            0
            all
