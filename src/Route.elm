module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Prng.Uuid exposing (Uuid)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)
import UuidSeed



-- ROUTING


type Route
    = TransactionList
    | TransactionNew
    | Transaction Uuid
    | CSV
    | SignOut


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map TransactionList Parser.top
        , Parser.map TransactionNew (s "transaction" </> s "new")
        , Parser.map Transaction (s "transaction" </> UuidSeed.urlParser)
        , Parser.map CSV (s "csv")
        , Parser.map SignOut (s "sign-out")
        ]



-- public helpers


href : Route -> Attribute msg
href targetroute =
    Attr.href (routetostring targetroute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routetostring route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routetostring route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse routeParser url



-- internal


routetostring : Route -> String
routetostring page =
    "/" ++ String.join "/" (routetopath page)


routetopath : Route -> List String
routetopath page =
    case page of
        TransactionList ->
            []

        TransactionNew ->
            [ "transaction"
            , "new"
            ]

        Transaction id ->
            [ "transaction"
            , Prng.Uuid.toString id
            ]

        CSV ->
            [ "csv" ]

        SignOut ->
            [ "sign-out" ]
