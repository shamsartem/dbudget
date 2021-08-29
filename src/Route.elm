module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import TransactionId exposing (TransactionId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- ROUTING


type Route
    = TransactionList
    | TransactionNew
    | Transaction TransactionId
    | CSV


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map TransactionList Parser.top
        , Parser.map TransactionNew (s "transaction" </> s "new")
        , Parser.map Transaction (s "transaction" </> TransactionId.urlParser)
        , Parser.map CSV (s "csv")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse routeParser url



-- INTERNAL


routeToString : Route -> String
routeToString page =
    "/" ++ String.join "/" (routeToPath page)


routeToPath : Route -> List String
routeToPath page =
    case page of
        TransactionList ->
            []

        TransactionNew ->
            [ "transaction", "new" ]

        Transaction id ->
            [ "transaction", TransactionId.toString id ]

        CSV ->
            [ "csv" ]
