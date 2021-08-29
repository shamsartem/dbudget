module TransactionId exposing (TransactionId, decoder, encode, toHtml, toString, urlParser)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser



-- TYPES


type TransactionId
    = TransactionId String



-- CREATE


decoder : Decoder TransactionId
decoder =
    Decode.map TransactionId Decode.string



-- TRANSFORM


encode : TransactionId -> Value
encode (TransactionId id) =
    Encode.string id


toString : TransactionId -> String
toString (TransactionId id) =
    id


urlParser : Url.Parser.Parser (TransactionId -> a) a
urlParser =
    Url.Parser.custom "TRANSACTION_ID" (\str -> Just (TransactionId str))


toHtml : TransactionId -> Html msg
toHtml (TransactionId id) =
    Html.text id
