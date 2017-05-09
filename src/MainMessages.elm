module MainMessages exposing (..)


type Msg
    = AddAuthor
    | DeleteAuthor Int
    | UpdateAuthorFieldString Int Int String
    | UpdateAuthorFieldBool Int Int
    | AddAffiliation Int
    | UpdateInstitution Int Int String
    | UpdateCountry Int Int String
    | UpdateCity Int Int String
    | UpdateState Int Int String
    | DeleteAffiliation Int Int
    | SetFocusedIds Int Int
    | SetAffiliationKeyDown Int Int
    | SetAuthors String
    | NewClass String
