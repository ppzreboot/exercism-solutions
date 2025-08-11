module Bob exposing (hey)

--- Return without further computation once the result is determined.
--- Traverse only ONCE.

isQuestion : String -> Bool
isQuestion = String.endsWith "?"

isYelling : String -> Bool
isYelling input =
    let
        loop : List Char -> Bool -> Bool
        loop rest hasUpper =
            case rest of
                [] -> hasUpper
                first :: nextRest ->
                    if Char.isLower first
                    then False
                    else if hasUpper
                    then loop nextRest True
                    else loop nextRest (Char.isUpper first)
    in
        loop (String.toList input) False

hey : String -> String
hey remark =
    let
        trimed = String.trim remark
    in
        if String.isEmpty trimed
        then "Fine. Be that way!"
        else (
            case (isQuestion trimed, isYelling trimed) of
                (True, True) -> "Calm down, I know what I'm doing!"
                (True, False) -> "Sure."
                (False, True) -> "Whoa, chill out!"
                (False, False) -> "Whatever."
        )
