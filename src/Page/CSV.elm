module Page.CSV exposing (..)

import Array exposing (Array)
import Csv
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task
import View.Header as Header exposing (viewHeader)


cl : String -> String
cl elementAndOrModifier =
    "CSV_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


type CsvState
    = NoCsv
    | ParsingCsv
    | ParsedCsv (List (Array String))
    | CsvParsingError String


type alias Model =
    { csv : CsvState }


init : ( Model, Cmd Msg )
init =
    ( { csv = NoCsv }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "CSV page" ]
        [ viewHeader Header.CSV
        , div [ c "container" ]
            (case model.csv of
                NoCsv ->
                    [ button [ onClick CsvRequested, class "button" ] [ text "Import CSV" ]
                    , button [ onClick CsvExport, class "button", c "exportButton" ] [ text "Export CSV" ]
                    ]

                ParsingCsv ->
                    [ text "Parsing..." ]

                ParsedCsv _ ->
                    [ text "Succecssfully parsed CSV"
                    , button [ onClick CsvRequested, class "button" ] [ text "Import another CSV" ]
                    , button [ onClick CsvExport, class "button", c "exportButton" ] [ text "Export CSV" ]
                    ]

                CsvParsingError errorText ->
                    [ div [ c "error" ] [ text errorText ]
                    , button [ onClick CsvRequested, class "button" ] [ text "Import CSV" ]
                    , button [ onClick CsvExport, class "button", c "exportButton" ] [ text "Export CSV" ]
                    ]
            )
        ]



-- update


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | CsvParsed ()
    | CsvExport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( { model | csv = NoCsv }
            , Select.file [ "text/csv" ] CsvSelected
            )

        CsvSelected file ->
            ( { model | csv = ParsingCsv }
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            case Csv.parse content of
                Ok { records } ->
                    ( { model | csv = ParsedCsv (records |> List.map (\l -> Array.fromList l)) }
                    , Task.perform CsvParsed (Task.succeed ())
                    )

                Err list ->
                    ( { model
                        | csv =
                            CsvParsingError
                                (list
                                    |> List.foldl
                                        (\{ row, col } acc ->
                                            acc
                                                ++ "row: "
                                                ++ String.fromInt row
                                                ++ " column: "
                                                ++ String.fromInt col
                                                ++ "\n"
                                        )
                                        "Problems in:\n"
                                )
                      }
                    , Cmd.none
                    )

        CsvParsed _ ->
            ( model, Cmd.none )

        CsvExport ->
            ( model, Cmd.none )
