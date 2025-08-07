module ResistorColorTrio exposing (Color(..), label)


type Color
    = Black
    | Brown
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Violet
    | Grey
    | White

getValue : Color -> Int
getValue color =
    case color of
      Black -> 0
      Brown -> 1
      Red -> 2
      Orange -> 3
      Yellow -> 4
      Green -> 5
      Blue -> 6
      Violet -> 7
      Grey -> 8
      White -> 9

label : List Color -> String
label colors =
    let
        value = case colors of
            first :: second :: third :: _ -> ((getValue first) * 10 + (getValue second)) * (10^(getValue third))
            _ -> 0
        
        giga = value > 0 && modBy 1000000000 value == 0
        mega = value > 0 && modBy 1000000 value == 0
        kilo = value > 0 && modBy 1000 value == 0

        state :
            { unit: String
            , value: Int
            }
        state = case (giga, mega, kilo) of
            (True, _, _) -> { unit = "giga", value = value // 1000000000 }
            (False, True, _) -> { unit = "mega", value = value // 1000000 }
            (False, False, True) -> { unit = "kilo", value = value // 1000 }
            _ -> { unit = "", value = value }
    in
        (String.fromInt state.value) ++ " " ++ state.unit ++ "ohms"
