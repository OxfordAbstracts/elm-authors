module DummyTypes exposing (..)

import MainModel exposing (..)


dummyIniatialModel : Model
dummyIniatialModel =
    MainModel.initialModel


dummyModel : Model
dummyModel =
    { dummyIniatialModel
        | authors = dummyAuthors
        , authorMaxId = getMaxAuthorId dummyAuthors
        , focusedAffiliationId = 1
        , focusedAuthorId = 1
        , authorFields = [ dummyAuthorField1, dummyAuthorField2, dummyAuthorField3 ]
    }


dummyAuthors : List Author
dummyAuthors =
    [ dummyAuthor1, dummyAuthor2 ]


dummyAuthor1 : Author
dummyAuthor1 =
    assignMaxAffiliationId
        { fields = dummyFieldResponses
        , affiliations = dummyAffiliations
        , maxAffiliationId = 0
        , id = 1
        }


dummyAuthor2 : Author
dummyAuthor2 =
    assignMaxAffiliationId
        { fields = dummyFieldResponses
        , affiliations = dummyAffiliations
        , maxAffiliationId = 0
        , id = 2
        }


dummyFieldResponses : List AuthorFieldResponse
dummyFieldResponses =
    [ { id = 0
      , authorFieldId = 0
      , value = "Conor"
      }
    , { id = 1
      , authorFieldId = 1
      , value = "Campbell"
      }
    ]


dummyAuthorField1 : AuthorField
dummyAuthorField1 =
    { id = 0
    , title = "First Name"
    , description = "Please put your first name here"
    , inputType = StringType
    }


dummyAuthorField2 : AuthorField
dummyAuthorField2 =
    { id = 1
    , title = "Last Initial"
    , description = "Please put your last initial here"
    , inputType = StringType
    }


dummyAuthorField3 : AuthorField
dummyAuthorField3 =
    { id = 2
    , title = "Presenting Paper"
    , description = "are you presenting"
    , inputType = BoolType
    }


dummyAffiliations : List Affiliation
dummyAffiliations =
    [ dummyAffiliation1, dummyAffiliation2 ]


dummyAffiliation1 : Affiliation
dummyAffiliation1 =
    { institution = "Leeds University"
    , city = "Leeds"
    , country = "United Kingdom"
    , id = 1
    }


dummyAffiliation2 : Affiliation
dummyAffiliation2 =
    { institution = "UCL"
    , city = "London"
    , country = "United Kingdom"
    , id = 2
    }
