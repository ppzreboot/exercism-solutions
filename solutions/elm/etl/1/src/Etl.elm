module Etl exposing (transform)

import Dict exposing (Dict)


-- (points, List letter) -> List (letter, points)
f1 : (Int, List String) -> List (String, Int)
f1 (points, letters) =
    List.map
        (\letter -> (String.toLower letter, points))
        letters

transform : Dict Int (List String) -> Dict String Int
transform =
    Dict.toList
    >> List.map f1
    >> List.concat
    >> Dict.fromList
