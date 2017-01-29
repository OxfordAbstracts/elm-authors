module Main exposing (..)

import Countries
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)
import Utils exposing (dropDuplicates, onKeyDown)


-- MODEL


type alias Model =
    { authorMaxId : Int
    , authors : List Author
    , focusedAuthorId : Int
    , focusedAffiliationId : Int
    , lastAffiliationKey : Int
    }


initialModel : Model
initialModel =
    { authorMaxId = 0
    , authors = [ blankAuthor 0 ]
    , focusedAuthorId = 0
    , focusedAffiliationId = 0
    , lastAffiliationKey = -1
    }


type alias Author =
    { firstName : String
    , lastName : String
    , presenting : Bool
    , affiliations : List Affiliation
    , maxAffiliationId : Int
    , id : Int
    }


type alias Affiliation =
    { institution : String
    , city : String
    , country : String
    , id : Int
    }


blankAuthor : Int -> Author
blankAuthor id =
    Author "" "" False [ blankAffiliation 0 ] 1 id


blankAffiliation : Int -> Affiliation
blankAffiliation id =
    Affiliation "" "" "" id


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- MESSAGES


type Msg
    = AddAuthor
    | DeleteAuthor Int
    | UpdateFirstName Int String
    | UpdateLastName Int String
    | TogglePresenting Int
    | AddAffiliation Int
    | UpdateInstitution Int Int String
    | UpdateCountry Int Int String
    | UpdateCity Int Int String
    | DeleteAffiliation Int Int
    | SetFocusedIds Int Int
    | SetAffiliationKeyDown Int Int


type Focused
    = Simple
    | Sections
    | None



-- VIEW


stylesheet : Html Msg
stylesheet =
    let
        tag =
            "link"

        attrs =
            []

        children =
            []
    in
        node tag attrs children


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
          -- , nav model.authors
        , renderAuthors model.authors
        , div [] (renderDataLists (getBlurredAuthorAffiliations model))
        ]


getBlurredAuthorAffiliations : Model -> List Affiliation
getBlurredAuthorAffiliations model =
    model.authors
        |> List.filter (\a -> a.id /= model.focusedAuthorId)
        |> List.map .affiliations
        |> List.concat


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
        authorsLength =
            List.length affiliations

        indexList =
            List.range 1 authorsLength

        affilIndexTuples =
            List.map2 (,) affiliations indexList
    in
        div []
            [ affiliationsHeader
            , div [] (List.map (renderAffiliation authorId) affiliations)
            ]


affiliationsHeader : Html Msg
affiliationsHeader =
    div [ class "clearfix" ]
        []


renderAffiliation : Int -> Int -> Affiliation -> Html Msg
renderAffiliation authorId affiliation =
    div [ class "affiliation form__question-sub-section" ]
        [ div [ class "form__label" ] [ text ("Affiliation " ++ toString index) ]
        , div
            [ class "form__question-sub-section--inline" ]
            [ input
                [ class "inline-element"
                , list "institutions-list"
                , placeholder "Institution"
                , onInput (UpdateInstitution authorId affiliation.id)
                , onFocus (SetFocusedIds authorId affiliation.id)
                , onKeyDown (SetAffiliationKeyDown affiliation.id)
                , value affiliation.institution
                ]
                []
            , input
                [ class "inline-element"
                , list "cities-list"
                , placeholder "City"
                , onInput (UpdateCity authorId affiliation.id)
                , onFocus (SetFocusedIds authorId affiliation.id)
                , value affiliation.city
                ]
                []
            , select
                [ class "inline-element"
                , list "countries-list"
                , onInput (UpdateCountry authorId affiliation.id)
                , onFocus (SetFocusedIds authorId affiliation.id)
                , value affiliation.country
                ]
                (Countries.options affiliation.country)
            , div
                [ class "remove button button--secondary"
                , onClick (DeleteAffiliation authorId affiliation.id)
                ]
                [ text "Remove Affiliation" ]
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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAuthor ->
            ( { model
                | authorMaxId = model.authorMaxId + 1
                , authors = model.authors ++ [ blankAuthor (model.authorMaxId + 1) ]
              }
            , Cmd.none
            )

        DeleteAuthor id ->
            ( { model | authors = List.filter (\a -> a.id /= id) model.authors }
            , Cmd.none
            )

        UpdateFirstName id newName ->
            let
                updateFirstName author =
                    { author | firstName = newName }
            in
                updateAuthor model id updateFirstName

        UpdateLastName id newName ->
            let
                updateLastName author =
                    { author | lastName = newName }
            in
                updateAuthor model id updateLastName

        TogglePresenting id ->
            let
                togglePresenting author =
                    { author | presenting = not author.presenting }
            in
                updateAuthor model id togglePresenting

        AddAffiliation id ->
            let
                addAffiliation author =
                    { author
                        | maxAffiliationId = author.maxAffiliationId + 1
                        , affiliations = author.affiliations ++ [ blankAffiliation (author.maxAffiliationId + 1) ]
                    }
            in
                updateAuthor model id addAffiliation

        UpdateInstitution authorId affiliationId new ->
            let
                updateInstitution affiliation =
                    if (model.lastAffiliationKey == -1 && new /= "") || (new == "" && model.lastAffiliationKey == 8) then
                        let
                            matchingAffiliation =
                                getBlurredAuthorAffiliations model
                                    |> List.filter (\a -> a.institution == new)
                                    |> List.head
                                    |> Maybe.withDefault (Affiliation new affiliation.city affiliation.country affiliation.id)
                        in
                            { affiliation
                                | institution = matchingAffiliation.institution
                                , city = matchingAffiliation.city
                                , country = matchingAffiliation.country
                            }
                    else
                        { affiliation | institution = new }
            in
                ( { model
                    | authors = getAffiliationUpdate model authorId affiliationId updateInstitution
                    , lastAffiliationKey = -1
                  }
                , Cmd.none
                )

        UpdateCountry authorId affiliationId new ->
            let
                updateInstitution affiliation =
                    { affiliation | country = new }
            in
                updateAffiliation model authorId affiliationId updateInstitution

        UpdateCity authorId affiliationId new ->
            let
                updateInstitution affiliation =
                    { affiliation | city = new }
            in
                updateAffiliation model authorId affiliationId updateInstitution

        DeleteAffiliation authorId affiliationId ->
            let
                deleteAffiliation author =
                    { author
                        | affiliations = List.filter (\a -> a.id /= affiliationId) author.affiliations
                    }
            in
                updateAuthor model authorId deleteAffiliation

        SetFocusedIds authorId affiliationId ->
            ( { model
                | focusedAuthorId = authorId
                , focusedAffiliationId = affiliationId
              }
            , Cmd.none
            )

        SetAffiliationKeyDown affiliationId key ->
            ( { model
                | lastAffiliationKey = key
              }
            , Cmd.none
            )


updateAuthor : Model -> Int -> (Author -> Author) -> ( Model, Cmd Msg )
updateAuthor model id change =
    ( { model
        | authors = updateIfHasId model.authors id change
      }
    , Cmd.none
    )


updateAffiliation : Model -> Int -> Int -> (Affiliation -> Affiliation) -> ( Model, Cmd Msg )
updateAffiliation model authorId affiliationId change =
    let
        updateAffiliation author =
            { author
                | affiliations = (updateIfHasId author.affiliations affiliationId change)
            }
    in
        updateAuthor model authorId updateAffiliation


getAuthorUpdate model id change =
    updateIfHasId model.authors id change


getAffiliationUpdate model authorId affiliationId change =
    let
        updateAffiliation author =
            { author
                | affiliations = (updateIfHasId author.affiliations affiliationId change)
            }
    in
        getAuthorUpdate model authorId updateAffiliation


updateIfHasId list id change =
    let
        changeIfHasId a =
            if a.id == id then
                (change a)
            else
                a
    in
        List.map changeIfHasId list



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
