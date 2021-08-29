module Page exposing (Page(..), newTransactionId, view)

import Browser exposing (Document)
import Html exposing (..)
import Route exposing (Route(..))


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | TransactionList
    | CSV


{-| Take a page's Html and frames it with a header

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view : { title : String, content : Html msg } -> Document msg
view { title, content } =
    { title = title ++ " - dbudget"
    , body = [ content ]
    }


newTransactionId : String
newTransactionId =
    "newTransaction"
