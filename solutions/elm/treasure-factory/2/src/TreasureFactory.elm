module TreasureFactory exposing
    ( TreasureChest
    , getTreasure
    , makeChest
    , makeTreasureChest
    , secureChest
    , uniqueTreasures
    )


type TreasureChest treasure
    = TreasureChest String treasure


getTreasure : String -> TreasureChest a -> Maybe a
getTreasure passwordAttempt (TreasureChest password treasure) =
    if passwordAttempt == password then
        Just treasure
    else
        Nothing


type Chest treasure phantom
    = Chest String treasure


makeChest : String -> treasure -> Chest treasure {}
makeChest = Chest


secureChest : Chest treasure phantom -> Maybe (Chest treasure { phantom | securePassword: () })
secureChest (Chest pw t) =
    if String.length pw > 7
    then Just (Chest pw t)
    else Nothing


type alias UniqueChest treasure phantom = Chest treasure { phantom | uniqueTreasure: () }

uniqueTreasures : List (Chest treasure phantom) ->
    List (UniqueChest treasure phantom)
uniqueTreasures list =
    let
        tag : Chest treasure phantom -> UniqueChest treasure phantom
        tag (Chest pw t) = Chest pw t
        isUnique : (Chest treasure phantom) -> Bool
        isUnique (Chest _ newTreasure) =
            List.filter (\(Chest _ t) -> t == newTreasure) list
            |> List.length
            |> (==) 1
    in
        List.filter (\c -> isUnique c) list
        |> List.map (\c -> tag c)


makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : () } -> TreasureChest treasure
makeTreasureChest (Chest pw treasure)= 
    TreasureChest pw treasure

