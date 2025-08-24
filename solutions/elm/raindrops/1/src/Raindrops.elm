module Raindrops exposing (raindrops)
import Parser exposing (number)


raindrops : Int -> String
raindrops number =
    let
        pling =
            if modBy 3 number == 0 then "Pling" else ""
        plang =
            if modBy 5 number == 0 then "Plang" else ""
        plong =
            if modBy 7 number == 0 then "Plong" else ""
        p = pling ++ plang ++ plong
        n =
            if String.length p == 0 then
                String.fromInt number
            else
                ""
    in pling ++ plang ++ plong ++ n
