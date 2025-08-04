module TreasureFactory exposing (TreasureChest, getTreasure, makeChest, makeTreasureChest, secureChest, uniqueTreasures)


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


uniqueTreasures : List (Chest treasure phantom) ->
    List (Chest treasure { phantom | uniqueTreasure: () })
uniqueTreasures =
    List.map (\(Chest pw treasure) ->
        Chest pw treasure
    )


makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : () } -> TreasureChest treasure
makeTreasureChest (Chest pw treasure)= 
    TreasureChest pw treasure

