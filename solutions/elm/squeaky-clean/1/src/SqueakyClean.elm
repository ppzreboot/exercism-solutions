module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)


clean1 : String -> String
clean1 = String.replace " " "_"


clean2 : String -> String
clean2 =
    clean1 >>
    String.map (\c ->
        if c == '\n' || c == '\t' || c == '\r' then
            '\n'
        else c
    )
    >> String.replace "\n" "[CTRL]"

isAlpha : String -> Bool
isAlpha ch =
    String.length ch > 0 && String.toUpper ch /= String.toLower ch

clean3 : String -> String
clean3 =
    clean2 >>
    String.foldr
        (\leftCh (rightCh, resultStr) ->
            if leftCh == '-' && isAlpha rightCh then
                ("", (String.toUpper rightCh) ++ resultStr)
            else
                (String.fromChar leftCh, rightCh ++ resultStr)
        )
        ("", "")
    >> (\(leftCh, resultStr) -> leftCh ++ resultStr)


clean4 : String -> String
clean4 =
    clean3
    >> String.filter (\c -> not (Char.isDigit c))


clean : String -> String
clean =
    clean4
    >> String.filter (\c -> c < 'α' || c > 'ω')
