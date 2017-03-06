port module Ports exposing (..)


port authors : (String -> msg) -> Sub msg


port authorsClass : (String -> msg) -> Sub msg


port checkAuthorsComplete : String -> Cmd msg
