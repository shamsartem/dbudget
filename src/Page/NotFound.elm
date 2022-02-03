module Page.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Route



-- VIEW


view : Html msg
view =
    main_ [ class "NotFound" ]
        [ h1 [ class "NotFound_title" ]
            [ text "Page not found" ]
        , a
            [ Route.href Route.TransactionList
            , class "button"
            ]
            [ text "Go to main page" ]
        ]
