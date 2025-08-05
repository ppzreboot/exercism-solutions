module RotationalCipher exposing (rotate)

shift : Int -> Char -> Char
shift key =
    Char.toCode
    >> (+) key
    >> Char.fromCode

shiftBack : Char -> Char
shiftBack = shift -26

rotate : String -> Int -> String
rotate text shiftKey =
    let
        sk = shift (modBy 26 shiftKey)
    in
        String.toList text
        |> List.map (\c ->
            if Char.isUpper c then
                let shifted = sk c
                in
                    if shifted > 'Z'
                    then shiftBack shifted
                    else shifted
            else if Char.isLower c then
                let shifted = sk c
                in
                    if shifted > 'z'
                    then shiftBack shifted
                    else shifted
            else
                c
        )
        |> String.fromList
