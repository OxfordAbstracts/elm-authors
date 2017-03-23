module Decoders exposing (..)

import MainModel exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required, decode, hardcoded)


authorDecoder : Decoder Author
authorDecoder =
    decode Author
        |> required "authorFieldResponses" (list authorFieldResponseDecoder)
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


authors authorsList =
    decodeString authorsDecoder authorsList
        |> Result.withDefault [ blankAuthor 0 [ 0, 1, 2 ] ]


authorFieldDecoder : Decoder AuthorField
authorFieldDecoder =
    decode AuthorField
        |> required "id" int
        |> required "title" string
        |> required "description" string
        |> required "inputType" (map fieldTypeHelper string)
        |> required "questionType" string


authorFieldResponseDecoder : Decoder AuthorFieldResponse
authorFieldResponseDecoder =
    decode AuthorFieldResponse
        |> required "id" int
        |> required "authorFieldId" int
        |> required "value" string


fieldTypeHelper : String -> FieldType
fieldTypeHelper inputType =
    case inputType of
        "bool" ->
            BoolType

        _ ->
            StringType


authorFields : String -> List AuthorField
authorFields authorFields =
    decodeString (list authorFieldDecoder) authorFields
        |> Result.withDefault [ defaultAuthorField1, defaultAuthorField2, defaultAuthorField3 ]
