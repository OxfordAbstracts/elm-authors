module MainUpdate exposing (..)

import MainMessages exposing (..)
import MainModel exposing (..)


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

        UpdateInstitution authorId affiliationId input ->
            let

                updateInstitution affiliation =
                    if (model.lastAffiliationKey == -1 && input /= "") then
                        let
                            matchingAffiliation =
                                getBlurredAuthorAffiliations model
                                    |> List.filter (\a -> a.institution == input)
                                    |> List.head
                                    |> Maybe.withDefault (Affiliation input affiliation.city affiliation.country affiliation.id)
                        in
                            { affiliation
                                | institution = matchingAffiliation.institution
                                , city = matchingAffiliation.city
                                , country = matchingAffiliation.country
                            }
                    else
                        { affiliation | institution = input }
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


getBlurredAuthorAffiliations : Model -> List Affiliation
getBlurredAuthorAffiliations model =
    model.authors
        |> List.filter (\a -> a.id /= model.focusedAuthorId)
        |> List.map .affiliations
        |> List.concat


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
