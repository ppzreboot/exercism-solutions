module PipersPie exposing (doubleFactorial, factorial, pipersPi)


factorial : Int -> Int
factorial n =
    if n == 0 then
        1
    else if n == 1 then
        1
    else
        n * factorial (n - 1)


doubleFactorial : Int -> Int
doubleFactorial n =
    if n == 0 then
        1
    else if n == 1 then
        1
    else
        n * doubleFactorial (n - 2)


pipersPi : Int -> Float
pipersPi limit =
    case limit of
        0 -> 2
        _ ->
            2 * toFloat(factorial limit)
            / toFloat(doubleFactorial (2 * limit + 1))
            + (pipersPi (limit - 1))