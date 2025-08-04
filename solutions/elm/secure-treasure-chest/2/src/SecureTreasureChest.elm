module SecureTreasureChest exposing
    ( Password
    , SecureTreasureChest
    , createPassword
    , createTreasure
    , getTreasure
    )

type Password
    = Password String


type SecureTreasureChest treasure
    = SecureTreasureChest String treasure


createPassword : String -> Maybe Password
createPassword passwordCandidate =
    if (String.length passwordCandidate) < 8
    then Nothing
    else Just (Password passwordCandidate)


createTreasure : t -> Password -> SecureTreasureChest t
createTreasure treasure (Password pw) =
    SecureTreasureChest pw treasure


getTreasure : String -> SecureTreasureChest t -> Maybe t
getTreasure passwordAttempt (SecureTreasureChest pw treasure) =
    if passwordAttempt == pw
    then Just treasure
    else Nothing
