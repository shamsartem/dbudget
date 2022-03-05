module View.Header exposing (ActiveNav(..), view)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Route exposing (Route(..))


{-| Determines which header link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the header doesn't
have links for every page. Anything that's not part of the header falls
under Other.

-}
type ActiveNav
    = Other
    | TransactionList
    | CSV

baseClass : String
baseClass =
    "Header"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


view : ActiveNav -> Html msg
view page =
    let
        linkto =
            headerLink page
    in
    nav [ class baseClass ]
        [ linkto Route.TransactionList [ text "Transactions" ]
        , linkto Route.CSV [ text "CSV" ]
        , linkto Route.LogOut [ text "Log out" ]
        ]


headerLink : ActiveNav -> Route -> List (Html msg) -> Html msg
headerLink page route linkcontent =
    a
        [ c "link"
        , classList
            [ ( cl "link__active"
              , isActive page route
              )
            ]
        , Route.href route
        ]
        linkcontent


isActive : ActiveNav -> Route -> Bool
isActive nav route =
    case ( nav, route ) of
        ( TransactionList, Route.TransactionList ) ->
            True

        ( CSV, Route.CSV ) ->
            True

        _ ->
            False
