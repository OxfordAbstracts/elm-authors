module MainModelTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import MainModel


all : Test
all =
    describe "MainModel tests"
        [ test "assignMaxAffiliationId change the maxAffiliationId property on the author" <|
            \() ->
                let
                    testAuthor =
                        { dummyAuthor1
                            | affiliations = List.singleton dummyAffiliation1
                        }

                    author =
                        MainModel.assignMaxAffiliationId testAuthor
                in
                    Expect.equal author.maxAffiliationId 1
        ]
