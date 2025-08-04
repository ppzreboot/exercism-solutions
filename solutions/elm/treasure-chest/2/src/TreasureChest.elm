module TreasureChest exposing (..)


type TreasureChest treasure =
    TreasureChest String treasure


getTreasure: String -> TreasureChest t -> Maybe t
getTreasure passwordAttempt (TreasureChest pw t) =
    if pw == passwordAttempt
    then Just t
    else Nothing

multiplyTreasure: (t -> List t) -> TreasureChest t -> TreasureChest (List t)
multiplyTreasure multiplier (TreasureChest pw t) =
    TreasureChest pw (multiplier t)
