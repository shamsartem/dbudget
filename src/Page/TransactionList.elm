module Page.TransactionList exposing
    ( DialogModel
    , InitType(..)
    , Model
    , Msg
    , getStore
    , getTitle
    , init
    , update
    , view
    )

import Dialog.TransactionDialog as TransactionDialog
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, id, style)
import InfiniteList
import Prng.Uuid exposing (Uuid)
import Result exposing (Result(..))
import Route exposing (Route(..))
import Store exposing (Store)
import Time
import Transaction.Field exposing (Field(..))
import Transaction.Transactions as Transactions exposing (Transactions)
import Transaction.Utils as TransactionUtils
import View.Header as Header exposing (viewHeader)
import View.Input as Input


type alias DisplayedTransaction =
    { category : String
    , date : String
    , id : Uuid
    , isIncome : Bool
    , name : String
    , price : String
    , lastUpdated : Int
    }


type DialogModel
    = DialogModel TransactionDialog.Model
    | WithoutDialog Store.SignedInStore


type alias Model =
    { dialogModel : DialogModel
    , search : String
    , infList : InfiniteList.Model
    }


getTitle : Model -> String
getTitle { dialogModel } =
    case dialogModel of
        DialogModel transactionDialogModel ->
            TransactionDialog.getTitle transactionDialogModel

        WithoutDialog _ ->
            "Transactions"


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


type ListItem
    = Header String
    | Row DisplayedTransaction


cl : String -> String
cl elementAndOrModifier =
    "TransactionsList_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


transactionsToDisplayedTransactions : Transactions -> List DisplayedTransaction
transactionsToDisplayedTransactions transactions =
    let
        notDeletedTransactionDataList =
            Transactions.getNotDeletedTransactionDataList transactions

        decimalsDict =
            Transactions.getDecimalsDict transactions
    in
    notDeletedTransactionDataList
        |> List.map
            (\transactionData ->
                let
                    { isIncome, date, category, name, id, lastUpdated } =
                        transactionData

                    fullPrice =
                        Result.withDefault "Invalid price" (TransactionUtils.getFullPrice transactionData decimalsDict)
                in
                { isIncome = isIncome
                , price = fullPrice
                , date = date
                , category = category
                , name = name
                , id = id
                , lastUpdated = Time.posixToMillis lastUpdated
                }
            )


filterDisplayedTransactions : String -> List DisplayedTransaction -> List DisplayedTransaction
filterDisplayedTransactions search displayedTransactions =
    let
        searchLower =
            String.toLower search
    in
    if searchLower == "" then
        displayedTransactions

    else
        displayedTransactions
            |> List.filter
                (\{ date, category, name } ->
                    [ date, category, name ]
                        |> List.any
                            (\text ->
                                text
                                    |> String.toLower
                                    |> String.contains searchLower
                            )
                )


sortTransactions : List DisplayedTransaction -> List DisplayedTransaction
sortTransactions list =
    list
        |> List.sortWith
            (\a b ->
                if a.date > b.date then
                    LT

                else if a.date < b.date then
                    GT

                else if a.lastUpdated > b.lastUpdated then
                    LT

                else if a.lastUpdated < b.lastUpdated then
                    GT

                else
                    EQ
            )


itemHeight : Int
itemHeight =
    70


containerHeight : Int
containerHeight =
    2160


config : InfiniteList.Config ListItem Msg
config =
    InfiniteList.config
        { itemView = itemView
        , itemHeight = InfiniteList.withConstantHeight itemHeight
        , containerHeight = containerHeight
        }
        |> InfiniteList.withOffset 300
        |> InfiniteList.withKeepFirst 1


getMessageView : List DisplayedTransaction -> List DisplayedTransaction -> Html Msg
getMessageView availableTransactionsToDisplay sortedTransactions =
    let
        getMessage msg =
            div [ c "statusMessage" ] [ text msg ]
    in
    case availableTransactionsToDisplay of
        [] ->
            getMessage "You currently have no transactions. Add them by using \"+\" button in the bottom right corner of the screen"

        _ ->
            case sortedTransactions of
                [] ->
                    getMessage "No search results"

                _ ->
                    text ""


viewTransactions : Transactions -> Model -> List (Html Msg)
viewTransactions transactions model =
    let
        availableTransactionsToDisplay =
            transactions
                |> transactionsToDisplayedTransactions

        sortedTransactions =
            availableTransactionsToDisplay
                |> filterDisplayedTransactions model.search
                |> sortTransactions
    in
    [ div
        [ c "infList", InfiniteList.onScroll InfListMsg ]
        [ InfiniteList.view
            config
            model.infList
            (Header model.search
                :: (sortedTransactions
                        |> List.map Row
                   )
            )
        ]
    , getMessageView availableTransactionsToDisplay sortedTransactions
    ]


type InitType
    = Edit String
    | New
    | NoDialog


init : InitType -> Store.SignedInStore -> ( Model, Cmd Msg )
init initType signedInStore =
    let
        hasDialog ( dialogModel, dialogCmd ) =
            ( DialogModel dialogModel, Just dialogCmd )

        ( transactionDialogModel, transactionDialogMsg ) =
            case initType of
                New ->
                    TransactionDialog.init TransactionDialog.New signedInStore
                        |> hasDialog

                Edit id ->
                    TransactionDialog.init (TransactionDialog.Edit id) signedInStore
                        |> hasDialog

                NoDialog ->
                    ( WithoutDialog signedInStore, Nothing )
    in
    ( { dialogModel = transactionDialogModel
      , search = ""
      , infList = InfiniteList.init
      }
    , case transactionDialogMsg of
        Nothing ->
            Cmd.none

        Just cmd ->
            Cmd.map GotDialogMsg cmd
    )



-- VIEW


headerView : String -> Html Msg
headerView search =
    div [ c "header" ]
        [ viewHeader Header.TransactionList
        , form [ c "searchContainer" ]
            [ div [ c "search" ]
                [ Input.view
                    { label = "search"
                    , onInput = SearchInput
                    , onBlur = Nothing
                    , value = search
                    , required = False
                    , id = "login"
                    , hasPlaceholder = True
                    , otherAttributes = []
                    , textUnderInput = Input.NoText
                    , dirty = False
                    , maybeDatalist = Nothing
                    }
                ]
            ]
        ]


transactionItemView : DisplayedTransaction -> Html Msg
transactionItemView { date, category, name, price, id, isIncome } =
    div
        [ c "item"
        , style "height" (String.fromInt itemHeight ++ "px")
        , classList [ ( cl "item__isIncome", isIncome ) ]
        ]
        [ div [ c "itemSection" ]
            [ div [] [ text date ]
            , div [] [ text category ]
            ]
        , div [ c "itemSection" ]
            [ div [] [ text name ]
            , div [] [ text price ]
            ]
        , a [ Route.href (Route.Transaction id), c "itemLink" ]
            [ div
                [ class "visuallyHidden" ]
                [ text (name ++ price ++ date ++ category) ]
            ]
        ]


itemView : Int -> Int -> ListItem -> Html Msg
itemView _ _ item =
    case item of
        Header search ->
            headerView search

        Row displayedTransaction ->
            transactionItemView displayedTransaction


view : Model -> Html Msg
view model =
    div [ class "TransactionsList page" ]
        (List.concat
            [ viewTransactions
                (Store.transactions
                    (getSignedInStore model)
                )
                model
            , [ a
                    [ class "roundButton"
                    , Route.href Route.TransactionNew
                    , id Header.newtransactionid
                    ]
                    [ span [ attribute "aria-hidden" "true" ] [ text "+" ]
                    , span [ class "visuallyHidden" ] [ text "Add Transaction" ]
                    ]
              ]
            , [ case model.dialogModel of
                    WithoutDialog _ ->
                        text ""

                    DialogModel m ->
                        Html.map GotDialogMsg (TransactionDialog.view m)
              ]
            ]
        )



-- update


type Msg
    = SearchInput String
    | InfListMsg InfiniteList.Model
    | GotDialogMsg TransactionDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput str ->
            ( { model | search = str }, Cmd.none )

        InfListMsg infList ->
            ( { model | infList = infList }, Cmd.none )

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
