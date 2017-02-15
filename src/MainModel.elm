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
    , authorFields = [ defaultAuthorField1, defaultAuthorField2, defaultAuthorField3 ]
    }


type FieldType
    = BoolType
    | StringType


type alias AuthorField =
    { id : Int
    , name : String
    , inputType : FieldType
    }


type alias AuthorFieldResponse =
    { id : Int
    , authorFieldId : Int
    , value : String
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


defaultAuthorField0 : AuthorField
defaultAuthorField0 =
    AuthorField 0 "Default" StringType


defaultAuthorField1 : AuthorField
defaultAuthorField1 =
    AuthorField 0 "First Name" StringType


defaultAuthorField2 : AuthorField
defaultAuthorField2 =
    AuthorField 1 "Last Name" StringType


defaultAuthorField3 : AuthorField
defaultAuthorField3 =
    AuthorField 2 "Email" StringType


defaultAuthorFieldResponse1 : AuthorFieldResponse
defaultAuthorFieldResponse1 =
    AuthorFieldResponse 0 0 "Answer1"


defaultAuthorFieldResponse2 : AuthorFieldResponse
defaultAuthorFieldResponse2 =
    AuthorFieldResponse 1 1 "Answer2"


defaultAuthorFieldResponse3 : AuthorFieldResponse
defaultAuthorFieldResponse3 =
    AuthorFieldResponse 2 2 "Answer3"


blankAuthor : Int -> Author
blankAuthor id =
    Author [ defaultAuthorFieldResponse1, defaultAuthorFieldResponse2, defaultAuthorFieldResponse3 ] [ blankAffiliation 0 ] 1 id


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
