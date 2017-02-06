module MainModel exposing (..)


type alias Model =
    { authorMaxId : Int
    , authors : List Author
    , focusedAuthorId : Int
    , focusedAffiliationId : Int
    , lastAffiliationKey : Int
    , affiliationLimit : Int
    }


type alias Flags =
    { authorsList : String
    , affiliationLimit : Int
    }


initialModel : Model
initialModel =
    { authorMaxId = 0
    , authors = [ blankAuthor 0 ]
    , focusedAuthorId = 0
    , focusedAffiliationId = 0
    , lastAffiliationKey = -1
    , affiliationLimit =
        5
    }


type alias Author =
    { firstName : String
    , lastName : String
    , presenting : Bool
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


blankAuthor : Int -> Author
blankAuthor id =
    Author "" "" False [ blankAffiliation 0 ] 1 id


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
