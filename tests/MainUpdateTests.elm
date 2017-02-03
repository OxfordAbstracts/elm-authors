module MainUpdateTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import MainUpdate


all : Test
all =
    describe "MainUpdate tests"
        [ test "getBlurredAuthorAffiliations get a list of all the affiliations which are not focused on" <|
            \() ->
                let
                    updatedModel =
                        MainUpdate.getBlurredAuthorAffiliations dummyModel
                in
                    Expect.equal updatedModel ([ { institution = "Leeds University", city = "Leeds", country = "United Kingdom", id = 1 } ])
        ]
