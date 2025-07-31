module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

remainingTimeInMinutes: Int -> Int -> Int
remainingTimeInMinutes layer alreadyPassed =
  let
    expectedMinutesInOven = 40
    preparationTimeInMinites = layer * 2
  in
    preparationTimeInMinites + expectedMinutesInOven - alreadyPassed
