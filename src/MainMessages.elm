module MainMessages exposing (..)


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
