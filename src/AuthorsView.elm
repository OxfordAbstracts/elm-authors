module AuthorsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (dropDuplicates, onKeyDown)
import Countries
import MainMessages exposing (..)
import MainModel exposing (..)
import MainUpdate exposing (..)
import Stylesheet exposing (view)
import Encode exposing (..)


view : Model -> Html Msg
view model =
    let
        authors =
            Encode.authors (model.authors)
    in
        div []
            [ Stylesheet.view
              -- , nav model.authors
            , renderAuthors model.authors
            , input [ class "hidden", id "authorsArray", name "authorsArray", value authors ] [ text authors ]
            , div [] (renderDataLists (getBlurredAuthorAffiliations model))
            ]


renderAuthors : List Author -> Html Msg
renderAuthors authors =
    let
        authorsLength =
            List.length authors

        indexList =
            List.range 1 authorsLength

        authorIndexTuples =
            List.map2 (,) authors indexList
    in
        div [ class "" ]
            [ div [ class "" ] (List.map renderAuthor authorIndexTuples)
            , div [ class "button button--tertiary", onClick AddAuthor ] [ text "Add Author" ]
            ]


authorDataClass : String
authorDataClass =
    "ma2"


renderAuthor : ( Author, Int ) -> Html Msg
renderAuthor ( author, index ) =
    div [ class "author form__question-sub-section" ]
        [ div [ class "form__label" ] [ text ("Author " ++ toString index) ]
        , div [ class "form__question-sub-section--inline" ]
            [ div [ class "inline-element" ]
                [ label [ class "form__label" ] [ text "First Name" ]
                , input [ class "form__input first-name", onInput (UpdateFirstName author.id), value author.firstName ] []
                ]
            , div [ class "inline-element" ]
                [ label [ class "form__label" ] [ text "Last Name" ]
                , input [ class "form__input last-name", onInput (UpdateLastName author.id), value author.lastName ] []
                ]
            , div [ class "inline-element" ]
                [ label [ class "form__label" ] [ text "Presenting Author" ]
                , input [ class "form__checkbox is-presenting question-checkbox", onClick (TogglePresenting author.id), type_ "checkbox", checked (author.presenting) ] []
                ]
            ]
        , span [ class "remove button button--secondary" ]
            [ div
                [ onClick (DeleteAuthor author.id) ]
                [ text "Remove Author" ]
            ]
        , div [ class "affiliates-dropdowns" ]
            [ (renderAffiliations author.affiliations author.id) ]
        , div [ class "add-affiliation-to-author button button--tertiary" ]
            [ div [ onClick (AddAffiliation author.id) ]
                [ text "Add Affiliation to Author" ]
            ]
        ]


renderAffiliations : List Affiliation -> Int -> Html Msg
renderAffiliations affiliations authorId =
    let
        affiliationsLength =
            List.length affiliations

        indexList =
            List.range 1 affiliationsLength

        affilIndexTuples =
            List.map2 (,) affiliations indexList
    in
        div []
            [ affiliationsHeader
            , div [] (List.map (renderAffiliation authorId) affilIndexTuples)
            ]


affiliationsHeader : Html Msg
affiliationsHeader =
    div [ class "clearfix" ]
        []


renderAffiliation : Int -> ( Affiliation, Int ) -> Html Msg
renderAffiliation authorId ( affiliation, index ) =
    div [ class "affiliation form__question-sub-section" ]
        [ div [ class "form__label" ] [ text ("Affiliation " ++ toString index) ]
        , div
            [ class "remove button button--secondary"
            , onClick (DeleteAffiliation authorId affiliation.id)
            ]
            [ text "Remove Affiliation" ]
        , div
            [ class "form__question-sub-section--inline" ]
            [ div [ class "inline-element" ]
                [ label [ class "form__label" ] [ text "Institution" ]
                , input
                    [ class "form__input institution"
                    , list "institutions-list"
                    , name "institution"
                    , onInput (UpdateInstitution authorId affiliation.id)
                    , onFocus (SetFocusedIds authorId affiliation.id)
                    , onKeyDown (SetAffiliationKeyDown affiliation.id)
                    , value affiliation.institution
                    ]
                    []
                ]
            , div [ class "inline-element" ]
                [ label [ class "form__label" ] [ text "City" ]
                , input
                    [ class "city form__input"
                    , list "cities-list"
                    , name "city"
                    , onInput (UpdateCity authorId affiliation.id)
                    , onFocus (SetFocusedIds authorId affiliation.id)
                    , value affiliation.city
                    ]
                    []
                ]
            , div [ class "inline-element" ]
                [ label [ class "form__label" ] [ text "Country" ]
                , select
                    [ class "country form__input form__input--dropdown"
                    , list "countries-list"
                    , name "country"
                    , onInput (UpdateCountry authorId affiliation.id)
                    , onFocus (SetFocusedIds authorId affiliation.id)
                    , value affiliation.country
                    ]
                    (Countries.options affiliation.country)
                ]
            ]
        ]


renderDataLists affiliations =
    let
        renderOptions list =
            list |> dropDuplicates |> List.map renderOption
    in
        [ datalist [ id "institutions-list" ]
            (affiliations
                |> List.map .institution
                |> renderOptions
            )
        , datalist [ id "countries-list" ]
            (affiliations
                |> List.map .country
                |> renderOptions
            )
        , datalist [ id "cities-list" ]
            (affiliations
                |> List.map .city
                |> renderOptions
            )
        ]


renderOption x =
    option [ value x ] []
