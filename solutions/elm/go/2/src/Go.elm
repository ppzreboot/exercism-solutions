module Go exposing (..)

import Result exposing (andThen)
import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    game
        |> oneStonePerPointRule
        |> andThen libertyRule
        |> andThen koRule
        |> \result ->
            case result of
                Err msg -> { game | error = msg }
                Ok newGame ->
                    newGame
                        |> captureRule
                        |> changePlayer

