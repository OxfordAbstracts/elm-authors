module Encoders exposing (..)

import Json.Encode as JsonEncode


getAffilitionValue affiliation =
    JsonEncode.object
        [ ( "id", JsonEncode.int affiliation.id )
        , ( "institution", JsonEncode.string affiliation.institution )
        , ( "city", JsonEncode.string affiliation.city )
        , ( "country", JsonEncode.string affiliation.country )
        ]


getFieldValue field =
    JsonEncode.object
        [ ( "id", JsonEncode.int field.id )
        , ( "authorFieldId", JsonEncode.int field.authorFieldId )
        , ( "value", JsonEncode.string field.value )
        ]


getAuthorsValue authors =
    JsonEncode.list (List.map getAuthorValue authors)


getAuthorValue author =
    JsonEncode.object
        [ ( "id", JsonEncode.int author.id )
        , ( "fields", JsonEncode.list (List.map getFieldValue author.fields) )
        , ( "affiliations", JsonEncode.list (List.map getAffilitionValue author.affiliations) )
        , ( "maxAffiliationId", JsonEncode.int author.maxAffiliationId )
        ]


authors authors =
    let
        debug =
            Debug.log "authors" authors
    in
        JsonEncode.encode 0 (getAuthorsValue authors)
