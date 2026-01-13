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
    let k = min limit 150
    in
    case k of
        0 -> 2
        _ ->
            2 * toFloat(factorial k)
            / toFloat(doubleFactorial (2 * k + 1))
            + (pipersPi (k - 1))