module AuthorsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (dropDuplicates, onKeyDown)
import Countries
import MainMessages exposing (..)
import MainModel exposing (..)
import MainUpdate exposing (..)
import Encoders exposing (..)
import Json.Decode
import List.Extra exposing (indexedFoldr, updateAt)
import Exts.List exposing (chunk)


view : Model -> Html Msg
view model =
    let
        authors =
            Encoders.authors (model.authors)
    in
        div []
            [ renderAuthors model
            , input [ class "hidden", id "authorsArray", name "authorsArray", value authors ] [ text authors ]
            , div [] (renderDataLists (getBlurredAuthorAffiliations model))
            ]


renderAuthors : Model -> Html Msg
renderAuthors model =
    let
        authorIndexTuples =
            model.authors
                |> List.length
                |> List.range 1
                |> List.map2 (,) model.authors

        addAuthorButton =
            if model.authorLimit > (List.length model.authors) then
                div [ class "button button--tertiary", onClick AddAuthor ] [ text "Add Another Author" ]
            else
                div []
                    []
    in
        div [ class model.class ]
            [ div [ class "" ] (List.map (renderAuthor model) authorIndexTuples)
            , addAuthorButton
            ]


authorDataClass : String
authorDataClass =
    "ma2"


renderAuthor : Model -> ( Author, Int ) -> Html Msg
renderAuthor model ( author, index ) =
    let
        addAffiliationButton =
            if model.affiliationLimit > (List.length author.affiliations) then
                div [ class "add-affiliation-to-author button button--tertiary" ]
                    [ div [ onClick (AddAffiliation author.id) ]
                        [ text "Add Another Affiliation to Author" ]
                    ]
            else
                div []
                    []

        chunkifiedAuthorFields =
            chunk 3 model.authorFields
    in
        div [ class "author form__question-sub-section" ]
            [ div [ class "form__label" ] [ text ("Author " ++ toString index) ]
            , div [ class "form__question-sub-section--inline" ]
                --for each of the authorFields we want to add a div like this
                [ div [ class "form__question-sub-section--inline" ] (List.map (renderFieldResponsesLine model author.authorFieldResponses author.id) chunkifiedAuthorFields)
                ]
            , span [ class "remove button button--secondary" ]
                [ div
                    [ onClick (DeleteAuthor author.id) ]
                    [ text "Remove Author" ]
                ]
            , div [ class "" ]
                [ (renderAffiliations model author.affiliations author.id) ]
            , addAffiliationButton
            ]


renderFieldResponsesLine model authorFieldResponses authorId authorFieldLine =
    div [ class "form__question-sub-section form__question-sub-section--inline" ]
        (List.map (renderFieldResponses model authorFieldResponses authorId) authorFieldLine)


renderFieldResponses model authorFieldResponses authorId authorField =
    let
        authorFieldResponse =
            authorFieldResponses
                |> List.filter (\a -> a.authorFieldId == authorField.id)
                |> List.head
                |> Maybe.withDefault defaultAuthorFieldResponse1

        labelX =
            if authorField.description /= "" then
                label
                    [ class "form__label tooltip" ]
                    [ text authorField.title
                    , span [ class "tooltip__box" ] [ text authorField.description ]
                    ]
            else
                label
                    [ class "form__label" ]
                    [ text authorField.title ]

        inputHtml =
            if authorField.inputType == BoolType then
                -- checkbox
                div [ class "inline-element" ]
                    [ labelX
                    , input
                        [ type_ "checkbox"
                        , class "form__input"
                        , checked (authorFieldResponse.value == "true")
                        , onClick (UpdateAuthorFieldBool authorId authorField.id)
                        ]
                        []
                    ]
            else if authorField.inputType == SinglePresenterType then
                -- checkbox only one can be checked across all the authors
                div [ class "inline-element" ]
                    [ labelX
                    , input
                        [ type_ "checkbox"
                        , class "form__input"
                        , checked (authorFieldResponse.value == "true")
                          -- if one of the other(!) inputs with SinglePresenterType === checked then disable
                        , disabled (disableThePresentingCheckbox model (authorFieldResponse.value == "true") authorField.id)
                        , onClick (UpdateAuthorFieldBool authorId authorField.id)
                        ]
                        []
                    ]
            else
                div [ class "inline-element" ]
                    [ labelX
                    , input
                        [ type_ "text"
                        , class "form__input"
                        , onInput (UpdateAuthorFieldString authorId authorField.id)
                        , value authorFieldResponse.value
                        ]
                        []
                    ]
    in
        inputHtml


disableThePresentingCheckbox : Model -> Bool -> Int -> Bool
disableThePresentingCheckbox model checked presentingFieldId =
    let
        disabled =
            if checked then
                False
            else
                List.any isTrue (List.map (isAuthorPresenting presentingFieldId) model.authors)
    in
        disabled


isTrue : Bool -> Bool
isTrue presenting =
    presenting == True


isAuthorPresenting : Int -> Author -> Bool
isAuthorPresenting presentingFieldId author =
    let
        defaultResponse =
            { value = "false" }

        presentingFieldResponse =
            List.filter (isPresentingfield presentingFieldId) author.authorFieldResponses
                |> List.head
                |> Maybe.withDefault defaultAuthorFieldResponse1

        isPresenting =
            presentingFieldResponse.value == "true"
    in
        isPresenting


isPresentingfield : Int -> AuthorFieldResponse -> Bool
isPresentingfield presentingFieldId authorFieldResponse =
    authorFieldResponse.id == presentingFieldId


renderAffiliations : Model -> List Affiliation -> Int -> Html Msg
renderAffiliations model affiliations authorId =
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
            , div [] (List.map (renderAffiliation model authorId) affilIndexTuples)
            ]


affiliationsHeader : Html Msg
affiliationsHeader =
    div [ class "clearfix" ]
        []


renderAffiliation : Model -> Int -> ( Affiliation, Int ) -> Html Msg
renderAffiliation model authorId ( affiliation, index ) =
    let
        institutionDiv =
            if model.showInstitution then
                div [ class "inline-element" ]
                    [ label
                        [ class "form__label" ]
                        [ text "Institution" ]
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
            else
                text ""

        cityDiv =
            if model.showCity then
                div [ class "inline-element" ]
                    [ label
                        [ class "form__label" ]
                        [ text "City" ]
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
            else
                text ""

        countryDiv =
            if model.showCountry then
                div [ class "inline-element" ]
                    [ label [ class "form__label" ] [ text "Country" ]
                    , select
                        [ class "country form__input form__input--dropdown"
                        , list "countries-list"
                        , name "country"
                        , onChange (UpdateCountry authorId affiliation.id)
                        , onFocus (SetFocusedIds authorId affiliation.id)
                        , value affiliation.country
                        ]
                        (Countries.options affiliation.country)
                    ]
            else
                text ""
    in
        div [ class "affiliation form__question-sub-section" ]
            [ div [ class "form__label" ] [ text ("Affiliation " ++ toString index) ]
            , div
                [ class "remove button button--secondary"
                , onClick (DeleteAffiliation authorId affiliation.id)
                ]
                [ text "Remove Affiliation" ]
            , div
                [ class "form__question-sub-section--inline" ]
                [ institutionDiv
                , cityDiv
                , countryDiv
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


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string
