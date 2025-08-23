module EliudsEggs exposing (eggCount)

eggCount : Int -> Int
eggCount n =
    let
        -- TCO: tail call optimization
        tco : Int -> Int -> Int
        tco acc nn =
            if nn == 0 then
                acc
            else
                tco
                    (acc + (modBy 2 nn))
                    (nn // 2)
    in tco 0 n
