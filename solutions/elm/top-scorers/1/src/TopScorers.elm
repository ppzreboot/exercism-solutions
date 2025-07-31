module TopScorers exposing (..)

import Dict exposing (Dict)
import TopScorersSupport exposing (PlayerName)


updateGoalCountForPlayer : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
updateGoalCountForPlayer playerName playerGoalCounts =
    Debug.todo "implement updateGoalCountForPlayer function, to initialise or increment the goalcount for PlayerName"


aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    List.foldr
        (\newUserName oldDict ->
            Dict.update
                newUserName 
                (\oldScore ->
                    case oldScore of
                        Just score -> Just (score + 1)
                        Nothing -> Just 1
                )
                oldDict
        )
        Dict.empty
        playerNames


removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Dict.filter
        (\_ score -> score >= goalThreshold)
        playerGoalCounts


resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    Dict.map
        (\name score ->
            if playerName == name then 0 else score)
        playerGoalCounts


formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    let
        score = Dict.get playerName playerGoalCounts
    in
        formatPPz
            ( playerName
            , case score of
                Just number -> number
                Nothing -> 0
            )

formatPPz: (PlayerName, Int) -> String
formatPPz (playerName, score) = playerName ++ ": " ++ (String.fromInt score)


formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    Dict.toList players
    |> List.map formatPPz
    |> String.join ", "


addScorePPz : (String, Int) -> Dict String Int -> Dict String Int
addScorePPz (name, scoreIncr) dict =
    Dict.update
        name
        (\scoreOld ->
            Just (
                case scoreOld of
                    Just score -> score + scoreIncr
                    Nothing -> scoreIncr)
        )
        dict

combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    Dict.merge
        (\key newValue old ->
            Dict.update
                key
                (\_ -> Just newValue)
                old
        )
        (\key valueL valueR old ->
            Dict.update
                key
                (\_ -> Just (valueL + valueR))
                old
        )
        (\key newValue old ->
            Dict.update
                key
                (\_ -> Just newValue)
                old
        )
        game1
        game2
        Dict.empty