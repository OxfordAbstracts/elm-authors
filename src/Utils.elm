module Utils exposing (..)

import Set
import Tuple


-- UTILS


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
