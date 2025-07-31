module Go exposing (..)
import GoSupport exposing (..)

map: (input -> output) -> Result err input -> Result err output
map calc prevResult =
    case prevResult of
        Ok input -> Ok (calc input)
        Err msg -> Err msg

andThen: (input -> Result err output) -> Result err input -> Result err output
andThen onSuccess prevResult =
    case prevResult of
        Ok input -> onSuccess input
        Err msg -> Err msg

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