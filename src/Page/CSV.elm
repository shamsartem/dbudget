module Page.CSV exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import View.Header as Header exposing (viewHeader)


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


view : Model -> Html Msg
view _ =
    div [ class "CSV page" ]
        [ viewHeader Header.CSV
        , div
            [ class "CSV_stubText"
            ]
            [ text "CSV export and import comming soon..." ]
        ]



-- update


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )
