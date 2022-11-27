module Page.Stats exposing
    ( Model
    , Msg
    , getStore
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (focus)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Numeric.ArithmeticError exposing (ArithmeticError)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat as Nat
import Process
import Store exposing (SignedInData, Store)
import Task
import Transaction
import View.Checkbox as Checkbox
import View.Header as Header
import View.Input as Input


baseClass : String
baseClass =
    "Stats"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


type alias Model =
    { store : Store
    , startDate : String
    , endDate : String
    , currency : String
    , isIncome : Bool
    }


getStore : Model -> Store
getStore model =
    model.store


setStore : Store -> Model -> Model
setStore store model =
    { model | store = store }


init : Store -> Store.SignedInData -> ( Model, Cmd Msg )
init store signedInData =
    let
        sortedTransactionsDataList =
            getSortedTransactionsDataList signedInData
    in
    ( { store = store
      , startDate = ""
      , endDate = ""
      , currency = getCurrencies sortedTransactionsDataList |> List.head |> Maybe.withDefault ""
      , isIncome = False
      }
    , Cmd.none
    )


sortByPopularity : List String -> List String
sortByPopularity transactions =
    transactions
        |> List.foldl
            (\key acc ->
                case Dict.get key acc of
                    Just val ->
                        Dict.insert key (val + 1) acc

                    Nothing ->
                        Dict.insert key 0 acc
            )
            Dict.empty
        |> Dict.toList
        |> List.sortWith
            (\( _, a ) ( _, b ) ->
                if a > b then
                    LT

                else if a < b then
                    GT

                else
                    EQ
            )
        |> List.map (\( key, _ ) -> key)


getStringsContainingField :
    (Transaction.Data -> String)
    -> List Transaction.Data
    -> List String
getStringsContainingField getValue filteredTransactionData =
    filteredTransactionData
        |> List.map (\transaction -> getValue transaction)
        |> sortByPopularity


getCurrencies : List Transaction.Data -> List String
getCurrencies notDeletedTransactionDataList =
    getStringsContainingField
        .currency
        notDeletedTransactionDataList


type ChartDataError
    = ChartTransactionParseError Transaction.ParseError
    | ChartArithmeticError ArithmeticError


getSortedTransactionsDataList : SignedInData -> List Transaction.Data
getSortedTransactionsDataList signedInData =
    Transaction.getNotDeletedTransactionDataList signedInData.transactions |> List.sortBy .date


getStartMin : List Transaction.Data -> String
getStartMin sortedTransactionsDataList =
    List.head sortedTransactionsDataList
        |> Maybe.map .date
        |> Maybe.withDefault ""


getEndMax : List Transaction.Data -> String
getEndMax sortedTransactionsDataList =
    List.reverse sortedTransactionsDataList
        |> List.head
        |> Maybe.map .date
        |> Maybe.withDefault ""


hundredPercent : Float
hundredPercent =
    100


percentPrecisionFloat : Float
percentPrecisionFloat =
    100


view : Model -> Html Msg
view model =
    let
        store =
            getStore model
    in
    case store.signedInData of
        Nothing ->
            text "Not signed in"

        Just signedInData ->
            let
                sortedTransactionsDataList =
                    getSortedTransactionsDataList signedInData

                startMin =
                    getStartMin sortedTransactionsDataList

                endMax =
                    getEndMax sortedTransactionsDataList

                startMax =
                    if model.endDate == "" then
                        endMax

                    else
                        model.endDate

                endMin =
                    if model.startDate == "" then
                        ""

                    else
                        model.startDate

                filteredTransactions =
                    sortedTransactionsDataList
                        |> List.filter
                            (\transactionData ->
                                let
                                    date =
                                        transactionData.date
                                in
                                transactionData.isIncome
                                    == model.isIncome
                                    && date
                                    >= model.startDate
                                    && date
                                    <= model.endDate
                                    && (transactionData.currency
                                            == model.currency
                                       )
                            )

                decimalsDict =
                    Transaction.getDecimalsDict signedInData.transactions

                chartDataResult =
                    filteredTransactions
                        |> List.foldl
                            (\transaction accResult ->
                                case accResult of
                                    Ok acc ->
                                        case Transaction.getPrice transaction decimalsDict of
                                            Ok price ->
                                                case Dict.get transaction.category acc of
                                                    Just val ->
                                                        case Decimal.addBounded val price of
                                                            Ok newVal ->
                                                                Ok (Dict.insert transaction.category newVal acc)

                                                            Err error ->
                                                                Err (ChartArithmeticError error)

                                                    Nothing ->
                                                        Ok (Dict.insert transaction.category price acc)

                                            Err error ->
                                                Err (ChartTransactionParseError error)

                                    Err error ->
                                        Err error
                            )
                            (Ok Dict.empty)
            in
            div [ class baseClass, class "page" ]
                [ div [ c "pageWrapper", class "fullSize" ]
                    [ Header.view Header.Stats
                    , div [ c "container" ]
                        [ div [ c "checkboxContainer" ]
                            [ Checkbox.view
                                { label = "Is Income"
                                , onCheck = SetIsIncome
                                , checked = model.isIncome
                                , required = False
                                , id = "isIncome"
                                , otherAttributes = []
                                }
                            ]
                        , div [ c "inputContainer" ]
                            [ Input.view
                                { label = "Start date"
                                , onInput = StartDateInput
                                , onBlur = Nothing
                                , value = model.startDate
                                , required = True
                                , hasPlaceholder = False
                                , id = "startDate"
                                , otherAttributes =
                                    [ type_ "date"
                                    , Html.Attributes.min startMin
                                    , Html.Attributes.max startMax
                                    ]
                                , textUnderInput = Input.NoText
                                , dirty = False
                                , maybeDatalist = Nothing
                                , hasClearButton = False
                                }
                            , Input.view
                                { label = "End date"
                                , onInput = EndDateInput
                                , onBlur = Nothing
                                , value = model.endDate
                                , required = True
                                , hasPlaceholder = False
                                , id = "endDate"
                                , otherAttributes =
                                    [ type_ "date"
                                    , Html.Attributes.min endMin
                                    , Html.Attributes.max endMax
                                    ]
                                , textUnderInput = Input.NoText
                                , dirty = False
                                , maybeDatalist = Nothing
                                , hasClearButton = False
                                }
                            ]
                        , div [ c "inputContainer" ]
                            [ Input.view
                                { label = "Currency"
                                , onInput = CurrencyInput
                                , onBlur = Nothing
                                , value = model.currency
                                , required = True
                                , hasPlaceholder = False
                                , id = "currency"
                                , otherAttributes = []
                                , textUnderInput = Input.NoText
                                , dirty = False
                                , maybeDatalist =
                                    Just
                                        { list = getCurrencies filteredTransactions
                                        , onSelect = SelectedCurrency
                                        }
                                , hasClearButton = True
                                }
                            ]
                        , case chartDataResult of
                            Ok chartData ->
                                let
                                    chartDataList =
                                        Dict.toList chartData

                                    precision =
                                        List.head chartDataList
                                            |> Maybe.map Tuple.second
                                            |> Maybe.map Decimal.getPrecision
                                            |> Maybe.withDefault Nat.nat0

                                    sumResult =
                                        List.foldl
                                            (\( _, value ) accResult -> accResult |> Result.andThen (\acc -> Decimal.addBounded acc value))
                                            (Ok (Decimal.succeed RoundDown precision 0))
                                            chartDataList

                                    chartList =
                                        chartDataList
                                            |> List.map
                                                (\( category, value ) ->
                                                    ( category, value |> Decimal.toFloat )
                                                )
                                            |> List.sortBy (Tuple.second >> negate)
                                in
                                case sumResult of
                                    Ok sumDecimal ->
                                        div [ c "chartContainer" ]
                                            (div [ c "total" ] [ text ("Total: " ++ Decimal.toString sumDecimal ++ " " ++ model.currency) ]
                                                :: (chartList
                                                        |> List.map
                                                            (\( category, val ) ->
                                                                div [ c "category" ]
                                                                    [ text (String.fromFloat (toFloat (round (val / (sumDecimal |> Decimal.toFloat) * percentPrecisionFloat * hundredPercent)) / percentPrecisionFloat) ++ "%")
                                                                    , text (" " ++ category)
                                                                    , text (" (" ++ String.fromFloat val ++ " " ++ model.currency ++ ")")
                                                                    ]
                                                            )
                                                   )
                                            )

                                    Err _ ->
                                        text "Can't calculate such a large sum"

                            Err _ ->
                                text "Error when summing up transactions"
                        ]
                    ]
                ]



-- update


type Msg
    = StartDateInput String
    | EndDateInput String
    | CurrencyInput String
    | SelectedCurrency String
    | SetIsIncome Bool
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDateInput startDate ->
            ( { model | startDate = startDate }, Cmd.none )

        EndDateInput endDate ->
            ( { model | endDate = endDate }, Cmd.none )

        CurrencyInput currency ->
            ( { model | currency = currency }, Cmd.none )

        SelectedCurrency currency ->
            ( model
            , Cmd.batch
                [ Task.attempt (\_ -> NoOp) (focus "chartContainer")
                , Process.sleep 1
                    |> Task.perform (\_ -> CurrencyInput currency)
                ]
            )

        SetIsIncome isIncome ->
            ( { model | isIncome = isIncome }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
