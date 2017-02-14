module Decoders exposing (..)

import MainModel exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required, decode, hardcoded)


authorDecoder : Decoder Author
authorDecoder =
    decode Author
        |> required "fields" (list authorFieldDecoder)
        |> required "affiliations" (list affiliationDecoder)
        |> hardcoded 0
        |> required "id" int


authorsDecoder : Decoder (List Author)
authorsDecoder =
    list authorDecoder


affiliationDecoder : Decoder Affiliation
affiliationDecoder =
    decode Affiliation
        |> required "institution" string
        |> required "city" string
        |> required "country" string
        |> required "id" int


authors authorsList authorFields =
    decodeString authorsDecoder authorsList
        |> Result.withDefault [ blankAuthor 0 ]


authorFieldDecoder : Decoder AuthorField
authorFieldDecoder =
    decode AuthorField
        |> required "id" int
        |> required "name" string
        |> required "inputType" (map fieldTypeHelper string)


fieldTypeHelper : String -> FieldType
fieldTypeHelper inputType =
    case inputType of
        "bool" ->
            Bool

        _ ->
            String


authorFields : String -> List AuthorField
authorFields authorFields =
    decodeString (list authorFieldDecoder) authorFields
        |> Debug.log "result"
        |> Result.withDefault [ defaultAuthorFeild ]
