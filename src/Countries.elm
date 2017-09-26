module Countries exposing (options)

import Html exposing (..)
import Html.Attributes exposing (value, disabled, selected)


options : List String -> String -> List (Html msg)
options countries selectedValue =
    [ option [ value "", selected (selectedValue == "") ] [ text "" ] ]
        ++ List.map (renderOption selectedValue) countries


renderOption : String -> String -> Html msg
renderOption selectedValue country =
    option
        [ value country
        , selected (selectedValue == country)
        , disabled (String.left 1 country == "-")
        ]
        [ text country ]
