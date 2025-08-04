module TreasureChest exposing (..)


type TreasureChest treasure =
    TreasureChest String treasure


getTreasure: String -> TreasureChest t -> Maybe t
getTreasure passwordAttempt tc =
    case tc of
        TreasureChest pw t ->
            if pw == passwordAttempt
            then Just t
            else Nothing

multiplyTreasure: (t -> List t) -> TreasureChest t -> TreasureChest (List t)
multiplyTreasure multiplier treasureChest =
    case treasureChest of
        TreasureChest pw t -> TreasureChest pw (multiplier t)
