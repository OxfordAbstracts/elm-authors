### Elm-authors version 1.0.26

Run npm start to see this module in action, in the browser.

The code compiles into index.js

Index.html passes flags to the embed elm side of the application.

The flags are used to pre-populate fields in the authors and affiliations.

```
type alias Flags =
    { authorsList : String
    , affiliationLimit : Int
    , authorLimit : Int
    , authorFields : String
    , showInstitution : Bool
    , showCity : Bool
    , showCountry : Bool
    }
```

There are also ports available to send the information which has been filled in
to the main application, and to listen form changes in the CSS classes

The author fields are fully customizable.
The affiliation fields can be toggled on and off.

To build: `elm-make Main.elm --output=index.js`
