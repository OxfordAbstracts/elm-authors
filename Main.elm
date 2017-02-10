module Main exposing (..)

import Html exposing (..)
import MainModel exposing (..)
import MainUpdate exposing (..)
import MainMessages exposing (..)
import AuthorsView exposing (view)
import Decode exposing (..)
import Ports exposing (..)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.authorsClass NewClass



-- MAIN


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
