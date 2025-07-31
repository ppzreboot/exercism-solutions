module TisburyTreasureHunt exposing (..)

-- Consider defining a type alias for TreasureLocation,
-- Treasure, PlaceLocation and Place,
-- and using them in the function type annotations

type alias TreasureLocation = (Int, Char)
type alias PlaceLocation = (Char, Int)
type alias Treasure = (String, TreasureLocation)
type alias Place = (String, PlaceLocation)

placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation (c, i) = (i, c)


treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
    let
        (pc, pi) = placeLocation
        (ti, tc) = treasureLocation
    in
        pc == tc && pi == ti

countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures place treasures =
    treasures
    |> List.filter (\treasure ->
        let
            (_, t_loc) = treasure
            (t_i, t_c) = t_loc
            (_, (p_c, p_i)) = place
        in
            t_i == p_i && t_c == p_c
    )
    |> List.length


specialCaseSwapPossible : ( String, TreasureLocation ) -> ( String, PlaceLocation ) -> ( String, TreasureLocation ) -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case (foundTreasure, place, desiredTreasure) of
        ("Brass Spyglass", "Abandoned Lighthouse", _) -> True
        ("Amethyst Octopus", "Stormy Breakwater", "Crystal Crab") -> True
        ("Amethyst Octopus", "Stormy Breakwater", "Glass Starfish") -> True
        ("Vintage Pirate Hat", "Harbor Managers Office", "Model Ship in Large Bottle") -> True
        ("Vintage Pirate Hat", "Harbor Managers Office", "Antique Glass Fishnet Float") -> True
        _ -> False

