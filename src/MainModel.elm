module MainModel exposing (..)


type alias Model =
    { authorMaxId : Int
    , authors : List Author
    , focusedAuthorId : Int
    , focusedAffiliationId : Int
    , lastAffiliationKey : Int
    , affiliationLimit : Int
    , class : String
    , authorFields : List AuthorField
    }


type alias Flags =
    { authorsList : String
    , affiliationLimit : Int
    , authorFields : String
    }


initialModel : Model
initialModel =
    { authorMaxId = 0
    , authors = [ blankAuthor 0 ]
    , focusedAuthorId = 0
    , focusedAffiliationId = 0
    , lastAffiliationKey = -1
    , affiliationLimit = 5
    , class = "complete"
    , authorFields = [ defaultAuthorFeild ]
    }


type FieldType
    = Bool
    | Text String


type alias AuthorField =
    { id : Int
    , name : String
    , inputType : FieldType
    }


type alias AuthorFieldResponse =
    { authorFieldId : Int
    , value : FieldType
    }


type alias Author =
    { fields : List AuthorFieldResponse
    , affiliations : List Affiliation
    , maxAffiliationId : Int
    , id : Int
    }


type alias Affiliation =
    { institution : String
    , city : String
    , country : String
    , id : Int
    }


defaultAuthorFeild : AuthorField
defaultAuthorFeild =
    AuthorField 0 "default field" Bool


defaultAuthorFieldResponse : AuthorFieldResponse
defaultAuthorFieldResponse =
    AuthorFieldResponse 0 (Text "Answer")


blankAuthor : Int -> Author
blankAuthor id =
    Author (List.repeat 3 defaultAuthorFieldResponse) [ blankAffiliation 0 ] 1 id


blankAffiliation : Int -> Affiliation
blankAffiliation id =
    Affiliation "" "" "" id


assignMaxAffiliationId : Author -> Author
assignMaxAffiliationId author =
    let
        maxAffiliationId =
            author
                |> .affiliations
                |> List.map .id
                |> List.maximum
                |> Maybe.withDefault -1
    in
        { author
            | maxAffiliationId = maxAffiliationId
        }


getMaxAuthorId : List Author -> Int
getMaxAuthorId authors =
    authors
        |> List.map .id
        |> List.maximum
        |> Maybe.withDefault -1


convertAuthorsListForModel : List Author -> List Author
convertAuthorsListForModel authors =
    List.map assignMaxAffiliationId authors
