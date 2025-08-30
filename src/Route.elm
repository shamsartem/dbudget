module Route exposing (Route(..), fromUrl, href, pushUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Prng.Uuid exposing (Uuid)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- ROUTING


type Route
    = Landing
    | TransactionList
    | TransactionNew
    | Transaction Uuid
    | CSV
    | Stats


uuidUrlParser : Parser (Uuid -> a) a
uuidUrlParser =
    Parser.custom "TRANSACTION_ID" (\str -> Prng.Uuid.fromString str)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map TransactionList (s "transactions")
        , Parser.map TransactionNew (s "transactions" </> s "new")
        , Parser.map Transaction (s "transactions" </> uuidUrlParser)
        , Parser.map Stats (s "stats")
        , Parser.map CSV (s "csv")
        ]



-- public helpers


href : Route -> Attribute msg
href targetroute =
    Attr.href (routeToString targetroute)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse routeParser url



-- internal


routeToString : Route -> String
routeToString page =
    "/" ++ String.join "/" (routeToPath page)


routeToPath : Route -> List String
routeToPath page =
    case page of
        Landing ->
            []

        TransactionList ->
            [ "transactions" ]

        TransactionNew ->
            [ "transactions", "new" ]

        Transaction id ->
            [ "transactions", Prng.Uuid.toString id ]

        Stats ->
            [ "stats" ]

        CSV ->
            [ "csv" ]
