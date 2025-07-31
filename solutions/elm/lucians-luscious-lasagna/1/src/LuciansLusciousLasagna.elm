module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

-- TODO: define the expectedMinutesInOven constant
expectedMinutesInOven =
    40
-- TODO: define the preparationTimeInMinutes function
preparationTimeInMinutes l = l * 2
-- TODO: define the elapsedTimeInMinutes function
elapsedTimeInMinutes l o = o + (preparationTimeInMinutes l)