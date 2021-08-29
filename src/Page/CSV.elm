module Page.CSV exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Page
import View.Header exposing (viewHeader)


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view _ =
    { title = "CSV"
    , content =
        div [ class "CSV page" ]
            [ viewHeader Page.CSV
            , div
                [ class "CSV_stubText"
                ]
                [ text "CSV export and import comming soon...or never" ]
            ]
    }



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )
