module MainMessages exposing (..)


type Msg
    = AddAuthor
    | DeleteAuthor Int
    | UpdateAuthorFieldString Int Int String
    | UpdateAuthorFieldBool Int Int String
    | AddAffiliation Int
    | UpdateInstitution Int Int String
    | UpdateCountry Int Int String
    | UpdateCity Int Int String
    | DeleteAffiliation Int Int
    | SetFocusedIds Int Int
    | SetAffiliationKeyDown Int Int
    | SetAuthors String
    | NewClass String
