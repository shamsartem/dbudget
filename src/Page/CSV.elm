module Page.CSV exposing
    ( Model
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
import Dialog.TransactionDialog as TransactionDialog
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Store exposing (Store)
import Task
import Time exposing (Posix)
import Transaction
import View.Header as Header
import View.Loader as Loader


type DialogModel
    = DialogModel TransactionDialog.Model
    | WithoutDialog
        { store : Store
        }


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
    { dialogModel : DialogModel
    , csv : CsvState
    , timeNow : Posix
    }


getStore : Model -> Store
getStore model =
    case model.dialogModel of
        WithoutDialog m ->
            m.store

        DialogModel m ->
            m.store


setStore : Store -> Model -> Model
setStore store model =
    { model
        | dialogModel =
            case model.dialogModel of
                WithoutDialog m ->
                    WithoutDialog { m | store = store }

                DialogModel m ->
                    DialogModel { m | store = store }
    }


init : Store -> ( Model, Cmd Msg )
init store =
    let
        { invalidTransactionData } =
            store

        maybeFirstInvalidTransaction =
            invalidTransactionData
                |> List.head

        hasDialog ( dialogModel, dialogCmd ) =
            ( DialogModel dialogModel, Just dialogCmd )

        ( transactionDialogModel, transactionDialogMsg ) =
            case maybeFirstInvalidTransaction of
                Nothing ->
                    ( WithoutDialog { store = store }, Nothing )

                Just invalidTransaction ->
                    TransactionDialog.init
                        (TransactionDialog.Invalid invalidTransaction)
                        store
                        |> hasDialog
    in
    ( { dialogModel = transactionDialogModel
      , csv = NoCsv
      , timeNow = Time.millisToPosix 0
      }
    , case transactionDialogMsg of
        Nothing ->
            Cmd.none

        Just cmd ->
            Cmd.map GotDialogMsg cmd
    )


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
        , case model.dialogModel of
            WithoutDialog _ ->
                text ""

            DialogModel m ->
                Html.map GotDialogMsg (TransactionDialog.view m)
        ]



-- update


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | CsvParsed (List (Array String))
    | GotTimeNowAfterCsvParsed (List (Array String)) Posix
    | CsvExport
    | GotDialogMsg TransactionDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
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

                allInvalidTransactions =
                    List.concat [ invalidTransactionData, store.invalidTransactionData ]

                maybeFirstInvalidTransaction =
                    allInvalidTransactions |> List.head

                newTransactions =
                    Transaction.mergeTransactions
                        store.transactions
                        (Transaction.getTransactionsDict transactions)

                newStore =
                    { store
                        | uuidSeed = newUuidSeed
                        , transactions = newTransactions
                        , invalidTransactionData =
                            Maybe.withDefault
                                []
                                (List.tail allInvalidTransactions)
                    }

                updateBasedOnInvalidTransactionData dialogModel cmd =
                    ( { model | dialogModel = dialogModel }
                    , cmd
                    )
            in
            case maybeFirstInvalidTransaction of
                Nothing ->
                    updateBasedOnInvalidTransactionData
                        (WithoutDialog
                            { store = newStore
                            }
                        )
                        Cmd.none

                Just invalidTransaction ->
                    let
                        ( m, cm ) =
                            TransactionDialog.init
                                (TransactionDialog.Invalid invalidTransaction)
                                newStore
                    in
                    updateBasedOnInvalidTransactionData
                        (DialogModel m)
                        (Cmd.map GotDialogMsg cm)

        CsvExport ->
            let
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

        GotDialogMsg dialogMsg ->
            case model.dialogModel of
                WithoutDialog _ ->
                    ( model, Cmd.none )

                DialogModel dialogModel ->
                    let
                        ( newDialogModel, newDialogCmd ) =
                            TransactionDialog.update dialogMsg dialogModel
                    in
                    ( { model | dialogModel = DialogModel newDialogModel }
                    , Cmd.map GotDialogMsg newDialogCmd
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dialogModel of
        DialogModel m ->
            Sub.map GotDialogMsg (TransactionDialog.subscriptions m)

        _ ->
            Sub.none
