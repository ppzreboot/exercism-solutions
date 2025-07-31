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
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map String.toLower


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

pizzaParser : Parser Pizza
pizzaParser =
    Parser.succeed Pizza
        |= nameParser
        |. Parser.spaces
        |= vegetarianParser
        |. Parser.spaces
        |= ingredientsParser
        |. Parser.spaces
        |. Parser.keyword "-"
        |= priceParser

menuParser : Parser (List Pizza)
menuParser =
    Parser.sequence
    { start = ""
    , separator = "\n"
    , end = ""
    , spaces = Parser.spaces
    , trailing = Parser.Optional
    , item = pizzaParser
    }


oneIngredientParser : Parser String
oneIngredientParser =
    Parser.sequence
        { start = ""
        , separator = " "
        , end = ""
        , trailing = Parser.Optional
        , spaces = Parser.spaces
        , item = wordParser
        }

        |> Parser.andThen
            (\words ->
                case List.length words of
                    0 -> Parser.problem "xxx"
                    _ -> Parser.succeed (String.join " " words)
            )
