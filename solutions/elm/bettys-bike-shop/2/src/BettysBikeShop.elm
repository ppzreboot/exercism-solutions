module BettysBikeShop exposing (penceToPounds, poundsToString)

-- TODO: import the String module
import String

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat(pence) / 100

poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat(pounds)
