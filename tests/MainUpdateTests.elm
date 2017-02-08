module MainUpdateTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import MainUpdate
import MainMessages exposing (..)


all : Test
all =
    describe "MainUpdate tests"
        [ test "getBlurredAuthorAffiliations get a list of all the affiliations which are not focused on" <|
            \() ->
                let
                    blurredAuthorAffiliations =
                        MainUpdate.getBlurredAuthorAffiliations dummyModel
                in
                    Expect.equal blurredAuthorAffiliations dummyAffiliations
        , test "The updateInstitution case of the update function returns the model and Cmd tuple with the updated institution" <|
            \() ->
                let
                    tuple =
                        MainUpdate.update (UpdateInstitution 1 1 "New Institution") dummyModel

                    expectedDummyAuthor1 =
                        { dummyAuthor1
                            | affiliations = [{ dummyAffiliation1 | institution = "New Institution" }, dummyAffiliation2 ]
                        }

                    expectedDummyAuthors =
                        [expectedDummyAuthor1, dummyAuthor2]

                    expectedDummyModel =
                        { dummyModel
                            | authors = expectedDummyAuthors
                        }
                in
                    Expect.equal tuple ( expectedDummyModel, Cmd.none )
        , test """The updateInstitution case. The Institution, City and Country have all been updated as the lastAffiliationKey was a (-1)""" <|
            \() ->
                let
                    tuple =
                        MainUpdate.update (UpdateInstitution 1 1 "UCL") { dummyModel | lastAffiliationKey = -1 }

                    expectedDummyAuthor1 =
                        { dummyAuthor1
                            | affiliations = [{ dummyAffiliation2 | id = 1 }, dummyAffiliation2]
                        }

                    expectedDummyAuthors =
                        [expectedDummyAuthor1, dummyAuthor2]

                    expectedDummyModel =
                        { dummyModel
                            | authors = expectedDummyAuthors
                        }
                in
                    Expect.equal tuple ( expectedDummyModel, Cmd.none )
        ]
