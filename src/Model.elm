module Model exposing (Model, Msg(..))

import Time
import Types exposing (Item, PrData, PrId)


type alias Model =
    { token : String
    , urlInput : String
    , items : List Item
    , repos : List String
    , authors : List String
    , repoInput : String
    , authorInput : String
    , configOpen : Bool
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
    | RepoInputChanged String
    | AddRepoClicked
    | RemoveRepoClicked String
    | AuthorInputChanged String
    | AddAuthorClicked
    | RemoveAuthorClicked String
    | ToggleConfig
    | Tick Time.Posix
