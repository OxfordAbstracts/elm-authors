module MainModel exposing (..)


type alias Model =
    { authorMaxId : Int
    , authors : List Author
    , focusedAuthorId : Int
    , focusedAffiliationId : Int
    , lastAffiliationKey : Int
    , affiliationLimit : Int
    , authorLimit : Int
    , showInstitution : Bool
    , showCity : Bool
    , showCountry : Bool
    , class : String
    , authorFields : List AuthorField
    }


type alias Flags =
    { authorsList : String
    , affiliationLimit : Int
    , authorLimit : Int
    , authorFields : String
    , showInstitution : Bool
    , showCity : Bool
    , showCountry : Bool
    }


initialModel : Model
initialModel =
    { authorMaxId = 0
    , authors = [ blankAuthor 0 [ 0, 1, 2 ] ]
    , focusedAuthorId = 0
    , focusedAffiliationId = 0
    , lastAffiliationKey = -1
    , affiliationLimit = 5
    , authorLimit = 5
    , showInstitution = True
    , showCity = False
    , showCountry = False
    , class = "complete"
    , authorFields = [ defaultAuthorField1, defaultAuthorField2, defaultAuthorField3 ]
    }


type FieldType
    = BoolType
    | StringType
    | SinglePresenterType


type alias AuthorField =
    { id : Int
    , title : String
    , description : String
    , inputType : FieldType
    , questionType : String
    }


type alias AuthorFieldResponse =
    { id : Int
    , authorFieldId : Int
    , value : String
    }


type alias Author =
    { authorFieldResponses : List AuthorFieldResponse
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
    AuthorField 0 "Default" "This is the default description" StringType "default"


defaultAuthorField1 : AuthorField
defaultAuthorField1 =
    AuthorField 0 "First Name" "This is the first name description" StringType "default"


defaultAuthorField2 : AuthorField
defaultAuthorField2 =
    AuthorField 1 "Last Name" "This is the last name description" StringType "default"


defaultAuthorField3 : AuthorField
defaultAuthorField3 =
    AuthorField 2 "Presenting" "This is the Presenting description" BoolType "default"


defaultAuthorFieldResponse1 : AuthorFieldResponse
defaultAuthorFieldResponse1 =
    AuthorFieldResponse 0 2 ""



--could give blankAuthor a list of the field (question) ids
-- then we could make the author field responses out of those


blankAuthor : Int -> List Int -> Author
blankAuthor id authorFieldIds =
    let
        authorFieldIdIndexTuples =
            authorFieldIds
                |> List.length
                |> List.range 0
                |> List.map2 (,) authorFieldIds

        blankAuthorFieldResponses =
            List.map blankAuthorFieldResponse authorFieldIdIndexTuples
    in
        Author blankAuthorFieldResponses [ blankAffiliation 0 ] 1 id


blankAuthorFieldResponse : ( Int, Int ) -> AuthorFieldResponse
blankAuthorFieldResponse ( authorFieldId, index ) =
    AuthorFieldResponse index authorFieldId ""


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
