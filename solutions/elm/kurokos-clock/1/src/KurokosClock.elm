module KurokosClock exposing (Locale(..), showDateTime, showLocalDate, showLocalTime)

import Time exposing (Month(..), Posix, Zone)


type Locale
    = US
    | JP

getMonthStr : Month -> String
getMonthStr m =
    case m of
        Jan -> "1"
        Feb -> "2"
        Mar -> "3"
        Apr -> "4"
        May -> "5"
        Jun -> "6"
        Jul -> "7"
        Aug -> "8"
        Sep -> "9"
        Oct -> "10"
        Nov -> "11"
        Dec -> "12"

i2s : Int -> String
i2s = String.fromInt


showLocalDate : Locale -> Int -> Month -> Int -> String
showLocalDate locale year month day =
    case locale of
        US ->
            (getMonthStr month) ++ "/" ++ (i2s day) ++ "/" ++ (i2s year)
        JP ->
            (i2s year) ++ "年" ++ (getMonthStr month) ++ "月" ++ (i2s day) ++ "日"


pad0 : Int -> String
pad0 n =
    if n < 10 then "0" ++ (String.fromInt n)
    else String.fromInt n

showLocalTime : Locale -> Int -> Int -> String
showLocalTime locale hour minute =
    case locale of
        US ->
            if hour > 12 then
                (i2s (hour - 12)) ++ ":" ++ (pad0 minute) ++ " PM"
            else if hour == 0 then
                "12:" ++ (pad0 minute) ++ " AM"
            else if hour == 12 then
                "12:" ++ (pad0 minute) ++ " PM"
            else
                (i2s hour) ++ ":" ++ (pad0 minute) ++ " AM"
        JP ->
            (i2s hour) ++ "時" ++ (i2s minute) ++ "分"


showDateTime : Locale -> Zone -> Posix -> String
showDateTime locale zone posix =
    let
        year = Time.toYear zone posix
        month = Time.toMonth zone posix
        day = Time.toDay zone posix
        hour = Time.toHour zone posix
        minute = Time.toMinute zone posix
        date = showLocalDate locale year month day
        time = showLocalTime locale hour minute
    in
        case locale of
            US -> date ++ " " ++ time
            JP -> date ++ time
