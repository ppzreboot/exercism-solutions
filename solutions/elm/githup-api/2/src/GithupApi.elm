module GithupApi exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias User =
    { id : Int
    , name : Maybe String
    , login : String
    , avatarUrl : String
    , siteAdmin : Bool
    }


type Side
    = Left
    | Right


type alias Comment =
    { id : Int
    , pullRequestReviewId : Maybe Int
    , user : User
    , body : String
    , side : Side
    , links : Dict String String
    }

decodeId : Decoder Int
decodeId =
    Decode.field "id" Decode.int

decodeName : Decoder (Maybe String)
decodeName =
    Decode.maybe (
        Decode.field "name" Decode.string
    )

decodeUser : Decoder User
decodeUser =
    Decode.map5 User
        decodeId
        decodeName
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "site_admin" Decode.bool)

decodePullRequestReviewId : Decoder (Maybe Int)
decodePullRequestReviewId =
    -- maybe 允许“字段不存在”、“类型不对”、“null”，这几种都会返回 Nothing，而不会“错误”。
    -- 但是 maybe 太宽松了。至少大多数情况下，“类型不对”时，应该“错误”。
    -- 而 nullable 只允许“值为 null”。“字段不存在”、“类型不对”时，都会“错误”。
    -- Decode.maybe (
    --     Decode.field "pull_request_review_id" Decode.int
    -- )
    Decode.field "pull_request_review_id" (Decode.nullable Decode.int)

decodeSide : Decoder Side
decodeSide =
    Decode.field "side" Decode.string
        |> Decode.andThen
            (\val ->
                case val of
                    "LEFT" -> Decode.succeed Left
                    "RIGHT" -> Decode.succeed Right
                    _ -> Decode.fail "wrong string"
            )

decodeLinks : Decoder (Dict String String)
decodeLinks =
    -- Decode.string
    -- |> Decode.field "href"
    -- |> Decode.dict
    -- |> Decode.field "_links"
    Decode.field "_links" (
        Decode.dict (
            Decode.field "href" Decode.string
        )
    )

decodeComment : Decoder Comment
decodeComment =
    Decode.map6 Comment
        decodeId
        decodePullRequestReviewId
        (Decode.field "user" decodeUser)
        (Decode.field "body" Decode.string)
        decodeSide
        decodeLinks


decodeComments : Decoder (List Comment)
decodeComments =
    Decode.list decodeComment

encodeUser : User -> Value
encodeUser user =
    Encode.object
        [ ( "id", Encode.int user.id )
        , ( "name", (
            case user.name of
            Just name -> Encode.string name
            _ -> Encode.null
        ))
        , ( "login", Encode.string user.login )
        , ( "avatar_url", Encode.string user.avatarUrl )
        , ( "site_admin", Encode.bool user.siteAdmin )
        ]

encodeComment : Comment -> Value
encodeComment comment =
    Encode.object
        [ ( "id", Encode.int comment.id )
        , ( "pull_request_review_id"
          , case comment.pullRequestReviewId of
                Just id -> Encode.int id
                _ -> Encode.null
          )
        , ( "user", encodeUser comment.user)
        , ( "body", Encode.string comment.body )
        , ( "side", Encode.string
            (case comment.side of
                Left -> "LEFT"
                Right -> "RIGHT"
            )
        )
        ,
            ( "_links"
            , Encode.dict
                identity
                (\link -> Encode.object [("href", Encode.string link)])
                comment.links
            )
        ]