module PaolasPrestigiousPizza exposing
    ( Pizza
    , ingredientsParser
    , menuParser
    , oneIngredientParser
    , pizzaParser
    , priceParser
    , vegetarianParser
    , wordParser
    )

import Parser exposing ((|.), (|=), Parser)


type alias Pizza =
    { name : String
    , vegetarian : Bool
    , ingredients : List String
    , price : Int
    }


priceParser : Parser Int
priceParser =
    Parser.succeed (\n -> n)
        |= Parser.int
        |. Parser.symbol "€"


vegetarianParser : Parser Bool
vegetarianParser =
    Parser.oneOf
        [ Parser.succeed True
            |. Parser.keyword "(v)"
        , Parser.succeed False
        ]


wordParser : Parser String
wordParser =
    Parser.chompWhile (\c -> (Char.isAlpha c) || (c == ' '))
        |> Parser.getChompedString
        |> Parser.map (\word -> String.trim (String.toLower word))


ingredientsParser : Parser (List String)
ingredientsParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.spaces
        , trailing = Parser.Optional
        , item = oneIngredientParser
        }


nameParser: Parser String
nameParser =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map String.toLower

pizzaParser : Parser Pizza
pizzaParser =
    Parser.succeed Pizza
        |= nameParser
        |. Parser.spaces
        |= vegetarianParser
        |. Parser.spaces
        |. Parser.keyword ":"
        |. Parser.spaces
        |= ingredientsParser
        |. Parser.spaces
        |. Parser.keyword "-"
        |. Parser.spaces
        |= priceParser

menuParser : Parser (List Pizza)
menuParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.succeed ()
        , trailing = Parser.Optional
        , item = pizzaParser
        }

        |. Parser.end


oneIngredientParser : Parser String
oneIngredientParser =
    wordParser
        |> Parser.andThen
            (\ingrName ->
                case ingrName of
                    "" -> Parser.problem "empty string"
                    _ -> Parser.succeed ingrName
            )
