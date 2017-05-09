module Main exposing (..)

import Html exposing (..)
import MainModel exposing (..)
import MainUpdate exposing (..)
import MainMessages exposing (..)
import AuthorsView exposing (view)
import Decoders exposing (..)
import Ports exposing (..)


-- MODEL


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        --do we want to decode the authorFields first then pass them to the authors
        authorFields =
            Decoders.authorFields flags.authorFields

        authors =
            Decoders.authors flags.authorsList
        { affiliationLimit, authorLimit, showInstitution, showCity, showState, showCountry, class} = flags

        authorsWithBlankResponses =
            List.map (addBlankResponsesToAuthor authorFields) authors

        addBlankResponsesToAuthor authorFields author =
            { author
                | authorFieldResponses = List.map (addBlankResponseIfNoResponse author) authorFields
            }

        addBlankResponseIfNoResponse author authorField =
            let
                authorFieldResponse =
                    author.authorFieldResponses
                        |> List.filter (\a -> a.authorFieldId == authorField.id)
                        |> List.head
                        |> Maybe.withDefault (AuthorFieldResponse authorField.id authorField.id "")
            in
                authorFieldResponse

        model =
            { initialModel
                | authors =
                    convertAuthorsListForModel authorsWithBlankResponses
                , authorMaxId =
                    getMaxAuthorId authors
                , affiliationLimit = affiliationLimit
                , authorLimit = authorLimit
                , authorFields = authorFields
                , showInstitution = showInstitution
                , showCity = showCity
                , showState = showState
                , showCountry = showCountry
                , class = class
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
    Sub.batch
        [ Ports.authorsClass NewClass
        , Ports.authors SetAuthors
        ]



-- MAIN


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
