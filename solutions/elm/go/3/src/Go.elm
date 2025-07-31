module Go exposing (..)

import Result exposing (andThen, map)
import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    game
        |> oneStonePerPointRule
        |> map captureRule
        |> andThen libertyRule
        |> andThen koRule
        |> map changePlayer
        |> \result ->
            case result of
                Err msg -> { game | error = msg }
                Ok newGame -> newGame

