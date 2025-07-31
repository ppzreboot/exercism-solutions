module Go exposing (..)

import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    case oneStonePerPointRule game of
        Err msg ->
            { game | error = msg }
        Ok game1 ->
            case libertyRule (captureRule game1) of
                Err msg ->
                    { game1 | error = msg }
                Ok game3 ->
                    case koRule game3 of
                        Err msg ->
                            { game1 | error = msg }
                        Ok game4 ->
                            { game4
                            | player =
                                case game4.player of
                                    White -> Black
                                    Black -> White
                            }

