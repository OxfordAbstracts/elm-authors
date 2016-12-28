module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus)
import Utils exposing (dropDuplicates)


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
    Author "" "" False [] 0 id


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
            [ attribute "Rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "https://cdnjs.cloudflare.com/ajax/libs/pure/0.6.1/base-min.css"
            ]

        children =
            []
    in
        node tag attrs children


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , nav model.authors
        , renderAuthors model.authors
        , div [] (renderDataLists (getUnfocusedAffiliations model))
        ]


getUnfocusedAffiliations : Model -> List Affiliation
getUnfocusedAffiliations model =
    let
        getAffiliationsFromAuthor author =
            if author.id == model.focusedAuthorId then
                List.filter (\a -> a.id /= model.focusedAffiliationId) author.affiliations
            else
                author.affiliations
    in
        model.authors
            |> List.map getAffiliationsFromAuthor
            |> List.concat


nav : List Author -> Html Msg
nav authors =
    div [ class "clearfix mb2 white bg-black" ]
        [ div [ class "left p2" ] [ text "Authors" ] ]


renderAuthors : List Author -> Html Msg
renderAuthors authors =
    div [ class "max-width-4 mx-auto" ]
        [ div [ class "p1 m1 border" ]
            [ div [ class "clearfix" ] (List.concat (List.map renderAuthor authors))
            , button [ onClick AddAuthor ] [ text "Add Author" ]
            ]
        ]


colWidthClass : String
colWidthClass =
    "col col-3"


renderAuthor : Author -> List (Html Msg)
renderAuthor author =
    [ span [ class colWidthClass ]
        [ text "First Name: ", input [ onInput (UpdateFirstName author.id), value author.firstName ] [] ]
    , span [ class colWidthClass ]
        [ text "Last Name: ", input [ onInput (UpdateLastName author.id), value author.lastName ] [] ]
    , span [ class colWidthClass ]
        [ text "Presenting: ", input [ onClick (TogglePresenting author.id), type_ "checkbox", checked (author.presenting) ] [] ]
    , div [ class colWidthClass ]
        [ (renderAffiliations author.affiliations author.id) ]
    , div [ class colWidthClass ]
        [ button [ onClick (AddAffiliation author.id) ]
            [ text "Add Affiliation" ]
        ]
    , div [ class colWidthClass ]
        [ button [ onClick (DeleteAuthor author.id) ] [ text "x" ]
        ]
    ]


renderAffiliations : List Affiliation -> Int -> Html Msg
renderAffiliations affiliations authorId =
    div []
        [ affiliationsHeader
        , div [] (List.map (renderAffiliation authorId) affiliations)
        ]


affiliationsHeader : Html Msg
affiliationsHeader =
    div [ class "clearfix" ]
        []


renderAffiliation : Int -> Affiliation -> Html Msg
renderAffiliation authorId affiliation =
    div []
        [ input
            [ list "institutions-list"
            , placeholder "Institution"
            , onInput (UpdateInstitution authorId affiliation.id)
            , onFocus (SetFocusedIds authorId affiliation.id)
            , value affiliation.institution
            ]
            []
        , input
            [ list "countries-list"
            , placeholder "Country"
            , onInput (UpdateCountry authorId affiliation.id)
            , onFocus (SetFocusedIds authorId affiliation.id)
            , value affiliation.country
            ]
            []
        , input
            [ list "cities-list"
            , placeholder "City"
            , onInput (UpdateCity authorId affiliation.id)
            , onFocus (SetFocusedIds authorId affiliation.id)
            , value affiliation.city
            ]
            []
        , button [ onClick (DeleteAffiliation authorId affiliation.id) ] [ text "x" ]
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
                    { affiliation | institution = new }
            in
                updateAffiliation model authorId affiliationId updateInstitution

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
