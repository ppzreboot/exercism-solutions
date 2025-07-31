module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    Maybe.withDefault "Mighty Magician" name

revive : Player -> Maybe Player
revive player =
    case player.health of
        0 ->
            if player.level > 9 then
                Just { player | health = 100, mana = Just 100 }
            else
                Just { player | health = 100 }
        _ -> Nothing

castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Nothing ->
            let
                absHealth = player.health - manaCost
                resultHealth =
                    if absHealth < 0 then 0 else absHealth
            in
            ({ player | health = resultHealth }, 0)
        Just currentMena ->
            if currentMena > manaCost then
                ({ player | mana = Just (currentMena - manaCost) }, manaCost * 2)
            else
                (player, 0)
