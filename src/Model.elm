module Model exposing (Model, Msg(..))

import Time
import Types exposing (Item, PrData, PrId)


type alias Model =
    { token : String
    , urlInput : String
    , items : List Item
    , now : Time.Posix
    , error : Maybe String
    , highlight : Maybe PrId
    , lastRefresh : Maybe Time.Posix
    }


type Msg
    = TokenChanged String
    | ForgetToken
    | UrlChanged String
    | AddClicked
    | RemoveClicked PrId
    | RefreshClicked
    | GotPr PrId (Result String PrData)
    | Tick Time.Posix
