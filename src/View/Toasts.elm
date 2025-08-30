module View.Toasts exposing (Config, view)

import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class)


type alias Config =
    List String


baseClass : String
baseClass =
    "Toasts"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


view : Config -> Html msg
view config =
    div [ class baseClass, class "fullSize" ]
        (List.map
            (\toast -> div [ c "toast" ] [ text toast ])
            config
        )
