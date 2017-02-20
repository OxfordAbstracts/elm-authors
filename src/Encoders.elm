module Encoders exposing (..)

import Json.Encode as JsonEncode


getAffilitionValue affiliation =
    JsonEncode.object
        [ ( "id", JsonEncode.int affiliation.id )
        , ( "institution", JsonEncode.string affiliation.institution )
        , ( "city", JsonEncode.string affiliation.city )
        , ( "country", JsonEncode.string affiliation.country )
        ]


getAuthorsValue authors =
    JsonEncode.list (List.map getAuthorValue authors)


getAuthorValue author =
    JsonEncode.object
        [ ( "id", JsonEncode.int author.id )
        , ( "firstName", JsonEncode.string author.firstName )
        , ( "lastName", JsonEncode.string author.lastName )
        , ( "affiliations", JsonEncode.list (List.map getAffilitionValue author.affiliations) )
        , ( "isPresenting", JsonEncode.bool author.presenting )
        , ( "maxAffiliationId", JsonEncode.int author.maxAffiliationId )
        ]


authors authors =
    JsonEncode.encode 0 (getAuthorsValue authors)
