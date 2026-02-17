module Bob exposing (hey)

isQuestion : String -> Bool
isQuestion = String.trim >> (String.endsWith "?")

isYelling : String -> Bool
isYelling input =
    (String.any (\c -> Char.isUpper c) input)
    && (String.all (\c ->
        not (Char.isAlpha c) || (Char.isUpper c)
    ) input)

isEmpty : String -> Bool
isEmpty =
    String.all (\c -> c == ' ' || c == '\t' || c == '\n')

hey : String -> String
hey remark =
    let
        input = (isQuestion remark, isYelling remark, isEmpty remark)
    in
        case input of
            (_, _, True) -> "Fine. Be that way!"
            (True, True, _) -> "Calm down, I know what I'm doing!"
            (True, _, _) -> "Sure."
            (False, True, _) -> "Whoa, chill out!"
            _ -> "Whatever."
