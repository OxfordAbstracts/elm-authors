module DummyTypes exposing (..)

import MainModel exposing (..)
import MainUpdate
import List exposing (..)


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
    }


dummyAuthors : List Author
dummyAuthors =
    List.append (List.singleton dummyAuthor1) (List.singleton dummyAuthor2)


dummyAuthor1 : Author
dummyAuthor1 =
    assignMaxAffiliationId
        { firstName = "Conor"
        , lastName = "Campbell"
        , presenting = True
        , affiliations = dummyAffiliations
        , maxAffiliationId = 0
        , id = 1
        }


dummyAuthor2 : Author
dummyAuthor2 =
    assignMaxAffiliationId
        { firstName = "Rory"
        , lastName = "Campbell"
        , presenting = False
        , affiliations = dummyAffiliations
        , maxAffiliationId = 0
        , id = 2
        }


dummyAffiliations : List Affiliation
dummyAffiliations =
    List.append (List.singleton dummyAffiliation1) (List.singleton dummyAffiliation2)


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
