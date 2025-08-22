module BirdCount exposing (busyDays, hasDayWithoutBirds, incrementDayCount, today, total)


today : List Int -> Maybe Int
today counts =
    case counts of
        [] -> Nothing
        first :: _ -> Just first

incrementDayCount : List Int -> List Int
incrementDayCount counts =
    case counts of
        [] -> [1]
        first :: rest -> (first + 1) :: rest


hasDayWithoutBirds : List Int -> Bool
hasDayWithoutBirds counts =
    case counts of
        [] -> False
        first :: rest ->
            if first == 0 then
                True
            else
                hasDayWithoutBirds rest


total : List Int -> Int
total counts =
    case counts of
        [] -> 0
        first :: rest -> first + total rest


busyDays : List Int -> Int
busyDays counts =
    case counts of
        [] -> 0
        first :: rest ->
            if first > 4 then
                1 + busyDays rest
            else
                busyDays rest
