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
                div [ class "button button--secondary aa__add-author-button", onClick AddAuthor ] [ text "Add Another Author" ]
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
                a
                    [ class "add-affiliation-to-author button button--secondary"
                    , onClick (AddAffiliation author.id)
                    ]
                    [ text "+ Add Affiliation" ]
            else
                div []
                    []

        isFullWidth authorField =
            authorField.inputType == LongStringType

        -- needs to filter out the full width responses from the model.authorFields
        fullWidthAuthorFields =
            List.filter isFullWidth model.authorFields

        authorFields =
            List.filter (not << isFullWidth) model.authorFields

        chunkifiedAuthorFields =
            chunk 3 authorFields

        fieldsHtml =
            List.append
                (List.map (renderFieldResponsesLine model author.authorFieldResponses author.id) chunkifiedAuthorFields)
                (List.map (renderFullWidthFieldResponse author.authorFieldResponses author.id) fullWidthAuthorFields)
    in
        div [ class "author aa" ]
            [ div [ class "aa__dividing-title" ]
                [ span [ class "aa__subtitle" ]
                    [ text ("Author " ++ toString index) ]
                ]
            , a
                [ class "remove aa__remove-button aa__remove-button--top-indent button button--secondary"
                , onClick (DeleteAuthor author.id)
                ]
                [ text ("Remove Author") ]
            , div [ class "aa__sub-section aa__sub-section--table" ]
                --for each of the authorFields we want to add a div like this:
                [ div [ class "aa__field aa__field--tablecell" ]
                    fieldsHtml
                ]
            , div [ class "aa__inner-container" ]
                [ div [ class "aa__dividing-title aa__dividing-title--linebreak" ]
                    [ span [ class "aa__subtitle" ]
                        [ text ("Author " ++ toString index ++ " Affiliations") ]
                    ]
                , (renderAffiliations model author.affiliations author.id)
                , addAffiliationButton
                ]
            ]


renderFullWidthFieldResponse authorFieldResponses authorId authorField =
    let
        authorFieldResponse =
            authorFieldResponses
                |> List.filter (\a -> a.authorFieldId == authorField.id)
                |> List.head
                |> Maybe.withDefault defaultAuthorFieldResponse1

        requiredText =
            if authorField.mandatory == "on" then
                " (Required)"
            else
                ""

        maxCharactersString =
            " (Max 500 Characters)"

        label =
            if authorField.description /= "" then
                label
                    [ class "form__label tooltip"
                    , for (authorField.title)
                    ]
                    [ text (authorField.title ++ maxCharactersString ++ requiredText)
                    , span [ class "tooltip__box" ] [ text authorField.description ]
                    ]
            else
                label
                    [ class "form__label" ]
                    [ text (String.append authorField.title requiredText) ]

        inputHtml =
            div [ class "aa__field aa__field--tablecell" ]
                [ label
                , textarea
                    [ rows 5
                    , maxlength 500
                    , class "form__input--textarea-author-field"
                    , onInput (UpdateAuthorFieldString authorId authorField.id)
                    , value authorFieldResponse.value
                    ]
                    []
                ]
    in
        div [ class "aa__sub-section aa__sub-section--table" ]
            [ inputHtml ]


renderFieldResponsesLine model authorFieldResponses authorId authorFieldLine =
    div [ class "aa__sub-section aa__sub-section--table" ]
        (List.map (renderFieldResponse model authorFieldResponses authorId) authorFieldLine)


renderFieldResponse model authorFieldResponses authorId authorField =
    let
        authorFieldResponse =
            authorFieldResponses
                |> List.filter (\a -> a.authorFieldId == authorField.id)
                |> List.head
                |> Maybe.withDefault defaultAuthorFieldResponse1

        requiredText =
            if authorField.mandatory == "on" then
                " (Required)"
            else
                ""

        labelX =
            if authorField.description /= "" then
                label
                    [ class "form__label tooltip"
                    , for (authorField.title)
                    ]
                    [ text (String.append authorField.title requiredText)
                    , span [ class "tooltip__box" ] [ text authorField.description ]
                    ]
            else
                label
                    [ class "form__label" ]
                    [ text (String.append authorField.title requiredText) ]

        inputHtml =
            if authorField.inputType == BoolType then
                -- checkbox
                div [ class "aa__field aa__field--tablecell" ]
                    [ labelX
                    , input
                        [ type_ "checkbox"
                        , id (authorField.title)
                        , class "form__input"
                        , checked (authorFieldResponse.value == "true")
                        , onClick (UpdateAuthorFieldBool authorId authorField.id)
                        ]
                        []
                    ]
            else if authorField.inputType == SinglePresenterType then
                -- checkbox only one can be checked across all the authors
                div [ class "aa__field aa__field--tablecell" ]
                    [ labelX
                    , input
                        [ type_ "checkbox"
                        , id (authorField.title)
                        , class "form__input"
                        , checked (authorFieldResponse.value == "true")

                        -- if one of the other(!) inputs with SinglePresenterType === checked then disable
                        , disabled (disableThePresentingCheckbox model (authorFieldResponse.value == "true") authorField.id)
                        , onClick (UpdateAuthorFieldBool authorId authorField.id)
                        ]
                        []
                    ]
            else
                div [ class "aa__field aa__field--tablecell" ]
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
        div [] (List.map (renderAffiliation model authorId) affilIndexTuples)


renderAffiliation : Model -> Int -> ( Affiliation, Int ) -> Html Msg
renderAffiliation model authorId ( affiliation, index ) =
    let
        institutionRequiredText =
            if model.mandatoryInstitution then
                "Institution (Required)"
            else
                "Institution"

        institutionDiv =
            if model.showInstitution then
                div [ class "aa__field aa__field--tablecell" ]
                    [ label
                        [ class "form__label" ]
                        [ text institutionRequiredText ]
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

        cityRequiredText =
            if model.mandatoryCity then
                "City (Required)"
            else
                "City"

        cityDiv =
            if model.showCity then
                div [ class "aa__field aa__field--tablecell" ]
                    [ label
                        [ class "form__label" ]
                        [ text cityRequiredText ]
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

        stateRequiredText =
            if model.mandatoryState then
                "State (Required)"
            else
                "State"

        stateDiv =
            if model.showState then
                div [ class "aa__field aa__field--tablecell" ]
                    [ label
                        [ class "form__label" ]
                        [ text stateRequiredText ]
                    , input
                        [ class "state form__input"
                        , list "states-list"
                        , name "state"
                        , onInput (UpdateState authorId affiliation.id)
                        , onFocus (SetFocusedIds authorId affiliation.id)
                        , value affiliation.state
                        ]
                        []
                    ]
            else
                text ""

        countryRequiredText =
            if model.mandatoryCountry then
                "Country (Required)"
            else
                "Country"

        countryDiv =
            if model.showCountry then
                div [ class "aa__field aa__field--tablecell" ]
                    [ label [ class "form__label" ]
                        [ text countryRequiredText ]
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
        div [ class "aa__sub-section" ]
            [ div [ class "aa__dividing-title" ]
                [ span [ class "aa__subtitle aa__subtitle--small" ]
                    [ text ("Affiliation " ++ toString index) ]
                ]
            , a
                [ class "remove aa__remove-button button button--secondary"
                , onClick (DeleteAffiliation authorId affiliation.id)
                ]
                [ text "Remove Affiliation" ]
            , div [ class "aa__sub-section aa__sub-section--table" ]
                [ institutionDiv
                , cityDiv
                , stateDiv
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
