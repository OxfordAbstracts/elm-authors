module MainUpdate exposing (..)

import MainMessages exposing (..)
import MainModel exposing (..)
import Encoders
import Decoders
import Json.Decode as Json
import Ports exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        encodedAuthors =
            model.authors
                |> Encoders.authors
    in
        case msg of
            AddAuthor ->
                ( { model
                    | authorMaxId = model.authorMaxId + 1
                    , authors = model.authors ++ [ blankAuthor (model.authorMaxId + 1) (List.range 0 ((List.length model.authorFields) - 1)) ]
                  }
                , checkAuthorsComplete encodedAuthors
                )

            NewClass class ->
                ( { model | class = class }
                , Cmd.none
                )

            DeleteAuthor id ->
                ( { model | authors = List.filter (\a -> a.id /= id) model.authors }
                , checkAuthorsComplete encodedAuthors
                )

            UpdateAuthorFieldString authorId fieldId input ->
                let
                    change field =
                        { field | value = input }
                in
                    updateAuthorFieldResponse model authorId fieldId change

            UpdateAuthorFieldBool authorId fieldId ->
                let
                    change field =
                        if field.value == "true" then
                            { field | value = "false" }
                        else
                            { field | value = "true" }
                in
                    updateAuthorFieldResponse model authorId fieldId change

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

                    updatedAuthors =
                        getAffiliationUpdate model authorId affiliationId updateInstitution

                    encodedAuthors =
                        updatedAuthors
                            |> Encoders.authors
                in
                    ( { model
                        | authors = updatedAuthors
                        , lastAffiliationKey = -1
                      }
                    , checkAuthorsComplete encodedAuthors
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
                , checkAuthorsComplete encodedAuthors
                )

            SetAuthors encodedAuthors ->
                let
                    authors =
                        encodedAuthors
                            |> Json.decodeString Decoders.authorsDecoder
                            |> Result.withDefault model.authors
                in
                    ( { model
                        | authors = authors
                      }
                    , checkAuthorsComplete encodedAuthors
                    )


getBlurredAuthorAffiliations : Model -> List Affiliation
getBlurredAuthorAffiliations model =
    model.authors
        |> List.filter (\a -> a.id /= model.focusedAuthorId)
        |> List.map .affiliations
        |> List.concat


updateAuthor : Model -> Int -> (Author -> Author) -> ( Model, Cmd Msg )
updateAuthor model id change =
    let
        updatedAuthors =
            updateIfHasId model.authors id change

        encodedAuthors =
            updatedAuthors
                |> Encoders.authors
    in
        ( { model
            | authors = updatedAuthors
          }
        , checkAuthorsComplete encodedAuthors
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


updateAuthorFieldResponse : Model -> Int -> Int -> (AuthorFieldResponse -> AuthorFieldResponse) -> ( Model, Cmd Msg )
updateAuthorFieldResponse model authorId authorFieldId change =
    let
        updateAuthorFieldResponse author =
            { author
                | authorFieldResponses = (updateIfHasId author.authorFieldResponses authorFieldId change)
            }
    in
        updateAuthor model authorId updateAuthorFieldResponse


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
