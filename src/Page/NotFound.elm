module Page.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Route


baseClass : String
baseClass =
    "NotFound"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)



-- VIEW


view : Html msg
view =
    main_ [ class baseClass ]
        [ h1 [ c "title" ] [ text "Page not found" ]
        , a
            [ Route.href Route.TransactionList
            , class "button"
            ]
            [ text "Go to main page" ]
        ]
