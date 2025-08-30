module Page.CSV exposing
    ( CsvState
    , Model
    , Msg
    , getStore
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Csv
import Csv.Encode
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Store exposing (Store)
import Task
import Time exposing (Posix)
import Transaction
import View.Header as Header
import View.Loader as Loader


baseClass : String
baseClass =
    "CSV"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


type CsvState
    = NoCsv
    | ParsingCsv
    | ParsedCsv
    | CsvParsingError String


type alias Model =
    { store : Store
    , csv : CsvState
    , timeNow : Posix
    }


getStore : Model -> Store
getStore model =
    model.store


setStore : Store -> Model -> Model
setStore store model =
    { model | store = store }


init : Store -> ( Model, Cmd Msg )
init store =
    ( { store = store, csv = NoCsv, timeNow = Time.millisToPosix 0 }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class baseClass, class "page" ]
        [ Header.view Header.CSV
        , div [ c "container" ]
            (case model.csv of
                NoCsv ->
                    [ button [ onClick CsvRequested, class "button" ] [ text "Import CSV" ]
                    , button [ onClick CsvExport, class "button", c "exportButton" ] [ text "Export CSV" ]
                    ]

                ParsingCsv ->
                    [ Loader.view "Parsing..." ]

                ParsedCsv ->
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
    | CsvParsed (List (Array String))
    | GotTimeNowAfterCsvParsed (List (Array String)) Posix
    | CsvExport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        store : Store
        store =
            getStore model
    in
    case msg of
        CsvRequested ->
            ( { model | csv = NoCsv }
            , Select.file
                [ "text/csv" ]
                CsvSelected
            )

        CsvSelected file ->
            ( { model | csv = ParsingCsv }
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            case Csv.parse content of
                Ok { records } ->
                    ( { model
                        | csv =
                            ParsedCsv
                      }
                    , Task.perform CsvParsed
                        (Task.succeed
                            (records
                                |> List.map (\l -> Array.fromList l)
                            )
                        )
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

        CsvParsed records ->
            ( model
            , Time.now
                |> Task.perform (GotTimeNowAfterCsvParsed records)
            )

        GotTimeNowAfterCsvParsed records timeNow ->
            let
                { transactions, invalidTransactionData, newUuidSeed } =
                    Transaction.listOfRowsToTransactions
                        store.uuidSeed
                        timeNow
                        records

                allInvalidTransactions : List Transaction.Transaction
                allInvalidTransactions =
                    List.concat [ invalidTransactionData, store.invalidTransactionData ]

                newTransactions : Transaction.ValidatedTransactions
                newTransactions =
                    Transaction.mergeTransactions
                        store.transactions
                        (Transaction.getTransactionsDict transactions)

                newStore : Store
                newStore =
                    { store
                        | uuidSeed = newUuidSeed
                        , transactions = newTransactions
                        , invalidTransactionData =
                            List.append
                                store.invalidTransactionData
                                allInvalidTransactions
                    }
            in
            -- TODO: maybe redirect to invalid transaction page
            ( { model | store = newStore }, Cmd.none )

        CsvExport ->
            let
                csvString : String
                csvString =
                    store.transactions
                        |> Transaction.getTransactionsDict
                        |> Transaction.toListOfListsOfStrings
                        |> (\list ->
                                { headers = Transaction.csvHeaders
                                , records = list
                                }
                           )
                        |> Csv.Encode.toString
            in
            ( model
            , File.Download.string "transactions.csv" "text/csv" csvString
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
