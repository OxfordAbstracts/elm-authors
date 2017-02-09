module Decode exposing (..)

import MainModel exposing (..)
import Json.Decode as JsonDecode
import Json.Decode.Pipeline exposing (required, decode, hardcoded)


authorDecoder : JsonDecode.Decoder Author
authorDecoder =
    decode Author
        |> required "firstName" JsonDecode.string
        |> required "lastName" JsonDecode.string
        |> required "isPresenting" JsonDecode.bool
        |> required "affiliations" (JsonDecode.list affiliationDecoder)
        |> hardcoded 0
        |> required "id" JsonDecode.int


authorsDecoder : JsonDecode.Decoder (List Author)
authorsDecoder =
    JsonDecode.list authorDecoder


affiliationDecoder : JsonDecode.Decoder Affiliation
affiliationDecoder =
    decode Affiliation
        |> required "institution" JsonDecode.string
        |> required "city" JsonDecode.string
        |> required "country" JsonDecode.string
        |> required "id" JsonDecode.int


authors authorsList =
    JsonDecode.decodeString authorsDecoder authorsList
        |> Result.withDefault [ blankAuthor 0 ]
