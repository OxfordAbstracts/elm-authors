port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import MainUpdateTests


allTests : Test
allTests =
    describe "all Tests"
        [ MainUpdateTests.all ]


main : TestProgram
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
