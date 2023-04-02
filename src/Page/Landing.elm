module Page.Landing exposing
    ( Model
    , Msg
    , getStore
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (class)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Route
import Store exposing (Store)


baseClass : String
baseClass =
    "Landing"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


type alias Model =
    { store : Store }


getStore : Model -> Store
getStore model =
    model.store


setStore : Store -> Model -> Model
setStore store model =
    { model | store = store }


init : Store -> ( Model, Cmd Msg )
init store =
    ( { store = store }
    , Cmd.none
    )


view : Model -> Html Msg
view _ =
    div [ class baseClass, class "page" ]
        [ h1 [ c "title" ] [ text "OWN YOUR FINANCIAL DATA" ]
        , a [ c "start", Route.href Route.TransactionList ]
            [ text "Start" ]
        ]



-- update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
