module Page.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Route


baseClassName : String
baseClassName =
    "NotFound"


cl : String -> String
cl elementAndOrModifier =
    baseClassName ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)



-- VIEW


view : Html msg
view =
    main_ [ class baseClassName ]
        [ h1 [ c "title" ]
            [ text "Page not found" ]
        , a
            [ Route.href Route.TransactionList
            , class "button"
            ]
            [ text "Go to main page" ]
        ]
