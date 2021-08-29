module Page.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Route



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content =
        main_ [ class "NotFound" ]
            [ h1 []
                [ text "Page not found" ]
            , a
                [ Route.href Route.TransactionList, class "button" ]
                [ text "Go to main page" ]
            ]
    }
