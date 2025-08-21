module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform =
    Dict.toList -- => List (points, chars)
    >> List.concatMap (\(points, chars) ->
        List.map
            (\letter -> (String.toLower letter, points))
            chars
    )
    >> Dict.fromList
