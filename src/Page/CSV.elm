module Page.CSV exposing (Model, Msg, getStore, init, update, view)

import Array exposing (Array)
import Cred
import Csv
import Csv.Encode
import Dialog.TransactionDialog as TransactionDialog
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Port
import Store exposing (Store, invalidTransactionData)
import Task
import Time exposing (Posix)
import Transaction.Transactions as Transactions
import Transaction.Utils as TransactionUtils
import View.Header as Header exposing (viewHeader)


type DialogModel
    = DialogModel TransactionDialog.Model
    | WithoutDialog Store.SignedInStore


baseClassName : String
baseClassName =
    "CSV"


cl : String -> String
cl elementAndOrModifier =
    baseClassName ++ "_" ++ elementAndOrModifier


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


getSignedInStore : Model -> Store.SignedInStore
getSignedInStore { dialogModel } =
    case dialogModel of
        WithoutDialog sigedInStore ->
            sigedInStore

        DialogModel m ->
            TransactionDialog.getSignedInStore m


getStore : Model -> Store
getStore model =
    getSignedInStore model |> Store.signedInStoreToStore


init : Store.SignedInStore -> ( Model, Cmd Msg )
init signedInStore =
    let
        invalidTransactionData =
            signedInStore
                |> Store.invalidTransactionData

        maybeFirstInvalidTransaction =
            invalidTransactionData
                |> List.head

        hasDialog ( dialogModel, dialogCmd ) =
            ( DialogModel dialogModel, Just dialogCmd )

        ( transactionDialogModel, transactionDialogMsg ) =
            case maybeFirstInvalidTransaction of
                Nothing ->
                    ( WithoutDialog signedInStore, Nothing )

                Just invalidTransaction ->
                    TransactionDialog.init
                        (TransactionDialog.Invalid invalidTransaction)
                        (signedInStore
                            |> Store.updateSignedInData
                                (\s ->
                                    { s
                                        | invalidTransactionData =
                                            Maybe.withDefault
                                                []
                                                (List.tail invalidTransactionData)
                                    }
                                )
                        )
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
    div [ class baseClassName, class "page" ]
        [ viewHeader Header.CSV
        , div [ c "container" ]
            (case model.csv of
                NoCsv ->
                    [ button [ onClick CsvRequested, class "button" ] [ text "Import CSV" ]
                    , button [ onClick CsvExport, class "button", c "exportButton" ] [ text "Export CSV" ]
                    ]

                ParsingCsv ->
                    [ text "Parsing..." ]

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
        signedInStore =
            model |> getSignedInStore
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
                { password, username } =
                    Cred.credToCredData (signedInStore |> Store.cred)

                ( transactionsDict, invalidTransactionData, uuidSeed ) =
                    Transactions.listOfRowsToTransactionsDict
                        (signedInStore
                            |> Store.signedInStoreToStore
                            |> Store.uuidSeed
                        )
                        timeNow
                        records

                maybeFirstInvalidTransaction =
                    invalidTransactionData |> List.head

                newTransactions =
                    TransactionUtils.mergeTransactions
                        (Store.transactions signedInStore)
                        transactionsDict

                newSignedInStore =
                    signedInStore
                        |> Store.updateSignedInData
                            (\signedInData ->
                                { signedInData
                                    | transactions = newTransactions
                                    , invalidTransactionData =
                                        Maybe.withDefault
                                            []
                                            (List.tail invalidTransactionData)
                                }
                            )
                        |> Store.updateAlwaysInStoreSignedIn
                            (\s -> { s | uuidSeed = uuidSeed })

                updateBasedOnInvalidTransactionData dialogModel cmd =
                    ( { model | dialogModel = dialogModel }
                    , Cmd.batch
                        [ Port.updatedTransactions
                            (Transactions.toJsonValue newTransactions)
                            password
                            username
                        , cmd
                        ]
                    )
            in
            case maybeFirstInvalidTransaction of
                Nothing ->
                    updateBasedOnInvalidTransactionData
                        (WithoutDialog (getSignedInStore model))
                        Cmd.none

                Just invalidTransaction ->
                    let
                        ( m, cm ) =
                            TransactionDialog.init
                                (TransactionDialog.Invalid invalidTransaction)
                                newSignedInStore
                    in
                    updateBasedOnInvalidTransactionData
                        (DialogModel m)
                        (Cmd.map GotDialogMsg cm)

        CsvExport ->
            let
                csvString =
                    signedInStore
                        |> Store.transactions
                        |> Transactions.getTransactionsDict
                        |> Transactions.toListOfListsOfStrings
                        |> (\list ->
                                { headers =
                                    [ "Is Income"
                                    , "Date"
                                    , "Category"
                                    , "Name"
                                    , "Price"
                                    , "Amount"
                                    , "Description"
                                    , "Currency"
                                    , "Id"
                                    , "LastUpdate"
                                    , "IsDeleted"
                                    ]
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
