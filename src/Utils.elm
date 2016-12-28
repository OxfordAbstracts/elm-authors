module Utils exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Set
import Tuple


-- DATA


dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )
            else
                ( Set.insert next set, next :: acc )
    in
        List.foldl step ( Set.empty, [] ) list |> Tuple.second |> List.reverse



-- DOM


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
