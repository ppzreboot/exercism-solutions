module ValentinesDay exposing (..)

type Approval
    = Yes
    | No
    | Maybe

type Cuisine
    = Korean
    | Turkish

type Genre
    = Crime
    | Horror
    | Romance
    | Thriller

type Activity
    = BoardGame
    | Chill
    | Movie Genre
    | Restaurant Cuisine


rateActivity : Activity -> Approval
rateActivity activity =
    if activity == Movie Romance || activity == Restaurant Korean then Yes
    else if activity == Restaurant Turkish then Maybe
    else No
