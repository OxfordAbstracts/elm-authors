port module Main exposing (..)

import Html exposing (..)
import MainModel exposing (..)
import MainUpdate exposing (..)
import MainMessages exposing (..)
import AuthorsView exposing (view)
import Decode exposing (..)


-- MODEL


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        authors =
            Decode.authors flags.authorsList

        affiliationLimit =
            flags.affiliationLimit

        model =
            { initialModel
                | authors =
                    convertAuthorsListForModel authors
                , authorMaxId =
                    getMaxAuthorId authors
                , affiliationLimit = affiliationLimit
            }
    in
        ( model, Cmd.none )


type Focused
    = Simple
    | Sections
    | None



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ AuthorsView.view model
        ]



-- SUBSCRIPTIONS


port authors : (String -> msg) -> Sub msg


port checkAuthorsComplete : (Model -> msg) -> Sub msg


port suggestions : (List String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    authors SetAuthors



-- MAIN


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
