module Main exposing (..)

import Autocomplete
import Html exposing (Html, div, span, button, input, text, node, program)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias Model =
    { authorMaxId : Int
    , authors : List Author
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


initialModel : Model
initialModel =
    { authorMaxId = 0
    , authors = [ blankAuthor 0 ]
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
    | SetAutoState Autocomplete.Msg
    | DeleteAuthor Int
    | UpdateFirstName Int String
    | UpdateLastName Int String
    | TogglePresenting Int
    | AddAffiliation Int
    | UpdateInstitution Int Int String
    | UpdateCountry Int Int String
    | UpdateCity Int Int String
    | DeleteAffiliation Int Int



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
        , list model.authors
        ]


nav : List Author -> Html Msg
nav authors =
    div [ class "clearfix mb2 white bg-black" ]
        [ div [ class "left p2" ] [ text "Authors" ] ]


list : List Author -> Html Msg
list authors =
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
        [ affiliationsHeader, div [] (List.map (renderAffiliation authorId) affiliations) ]


affiliationsHeader : Html Msg
affiliationsHeader =
    div [ class "clearfix" ]
        []


renderAffiliation : Int -> Affiliation -> Html Msg
renderAffiliation authorId affiliation =
    div []
        [ input [ placeholder "Institution", onInput (UpdateInstitution authorId affiliation.id), value affiliation.institution ] []
        , input [ placeholder "Country", onInput (UpdateCountry authorId affiliation.id), value affiliation.country ] []
        , input [ placeholder "City", onInput (UpdateCity authorId affiliation.id), value affiliation.city ] []
        , button [ onClick (DeleteAffiliation authorId affiliation.id) ] [ text "x" ]
        ]



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

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState (acceptablePeople model.query model.people)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel


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


updateConfig : Autocomplete.UpdateConfig Msg Person
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewPerson maybeId
                else if code == 13 then
                    Maybe.map SelectPersonKeyboard maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewPerson id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectPersonMouse id
        , separateSelections = False
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PEOPLE


type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    }


presidents : List Person
presidents =
    [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
    , Person "John Adams" 1735 "Braintree" "Massachusetts"
    , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
    , Person "James Madison" 1751 "Port Conway" "Virginia"
    , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
    , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
    , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
    , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
    , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
    , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
    , Person "John Tyler" 1790 "Charles City County" "Virginia"
    , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
    , Person "James K. Polk" 1795 "Pineville" "North Carolina"
    , Person "Millard Fillmore" 1800 "Summerhill" "New York"
    , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
    , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
    , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
    , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
    , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
    , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
    , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
    , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
    , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
    , Person "William McKinley" 1843 "Niles" "Ohio"
    , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
    , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
    , Person "Theodore Roosevelt" 1858 "New York City" "New York"
    , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
    , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
    , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
    , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
    , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
    , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
    , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
    , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
    , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
    , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
    , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
    , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
    , Person "Jimmy Carter" 1924 "Plains" "Georgia"
    , Person "George W. Bush" 1946 "New Haven" "Connecticut"
    , Person "Bill Clinton" 1946 "Hope" "Arkansas"
    , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
    ]
