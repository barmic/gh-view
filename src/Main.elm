port module Main exposing (main)

{-| Wiring: state, update, ports, subscriptions. Rendering lives in `View`.
-}

import Browser
import Config
import Discover
import Favicon
import GitHub
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (Model, Msg(..))
import Persist
import PrUrl
import Time
import Types exposing (Item)
import View



-- PORTS (out): mirror state into localStorage


port storeToken : String -> Cmd msg


port storePrs : Encode.Value -> Cmd msg


port storeRepos : List String -> Cmd msg


port storeAuthors : List String -> Cmd msg



-- PORTS (out): reflect the open-PR count into the favicon and tab title


port setFavicon : String -> Cmd msg


port setTitle : String -> Cmd msg


{-| Emit favicon + title for the current open-PR count. Called whenever the
list (or an item's finished-ness) changes.
-}
badgeCmds : List Item -> List (Cmd Msg)
badgeCmds items =
    let
        openCount =
            items |> List.filter (not << Types.isFinished) |> List.length
    in
    [ setFavicon (Favicon.dataUri openCount), setTitle (Favicon.title openCount) ]



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = View.view
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        flags =
            Persist.decodeFlags flagsValue

        base =
            { token = flags.token
            , urlInput = ""
            , items = flags.items
            , repos = flags.repos
            , authors = flags.authors
            , repoInput = ""
            , authorInput = ""
            , configOpen = List.isEmpty flags.repos && List.isEmpty flags.authors
            , discovering = False
            , now = flags.now
            , error = Nothing
            , notice = Nothing
            , highlight = Nothing
            , lastRefresh =
                if String.isEmpty flags.token then
                    Nothing

                else
                    Just flags.now
            }

        ( items, cmd ) =
            refreshAll base
    in
    ( { base | items = items, error = loadWarning base }
    , Cmd.batch (cmd :: badgeCmds items)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TokenChanged token ->
            ( { model | token = token }, storeToken token )

        ForgetToken ->
            ( { model | token = "" }, storeToken "" )

        UrlChanged input ->
            ( { model | urlInput = input, highlight = Nothing, error = Nothing }, Cmd.none )

        AddClicked ->
            add model

        RemoveClicked id ->
            let
                items =
                    List.filter (\it -> not (Types.sameId it.id id)) model.items
            in
            ( { model | items = items }
            , Cmd.batch (storePrs (Persist.encodeItems items) :: badgeCmds items)
            )

        RefreshClicked ->
            if String.isEmpty model.token then
                ( { model | error = Just tokenMissing }, Cmd.none )

            else
                let
                    ( items, cmd ) =
                        refreshAll model
                in
                ( { model | items = items, error = Nothing, notice = Nothing, lastRefresh = Just model.now }, cmd )

        GotPr id result ->
            case result of
                Err message ->
                    ( { model | error = Just message, items = stopFetching id model.items }, Cmd.none )

                Ok data ->
                    let
                        items =
                            List.map
                                (\it ->
                                    if Types.sameId it.id id then
                                        { it | data = Just data, fetching = False }

                                    else
                                        it
                                )
                                model.items
                    in
                    ( { model | items = items }
                    , Cmd.batch (storePrs (Persist.encodeItems items) :: badgeCmds items)
                    )

        RepoInputChanged input ->
            ( { model | repoInput = input, error = Nothing }, Cmd.none )

        AddRepoClicked ->
            case Config.parseRepo model.repoInput of
                Nothing ->
                    if String.isEmpty (String.trim model.repoInput) then
                        ( model, Cmd.none )

                    else
                        ( { model | error = Just repoInvalid }, Cmd.none )

                Just slug ->
                    let
                        repos =
                            Config.addItem slug model.repos
                    in
                    ( { model | repos = repos, repoInput = "", error = Nothing }
                    , storeRepos repos
                    )

        RemoveRepoClicked slug ->
            let
                repos =
                    Config.removeItem slug model.repos
            in
            ( { model | repos = repos }, storeRepos repos )

        AuthorInputChanged input ->
            ( { model | authorInput = input, error = Nothing }, Cmd.none )

        AddAuthorClicked ->
            let
                login =
                    Config.normalizeAuthor model.authorInput
            in
            if String.isEmpty login then
                ( model, Cmd.none )

            else
                let
                    authors =
                        Config.addItem login model.authors
                in
                ( { model | authors = authors, authorInput = "", error = Nothing }
                , storeAuthors authors
                )

        RemoveAuthorClicked login ->
            let
                authors =
                    Config.removeItem login model.authors
            in
            ( { model | authors = authors }, storeAuthors authors )

        ToggleConfig ->
            ( { model | configOpen = not model.configOpen }, Cmd.none )

        FetchNewClicked ->
            if String.isEmpty model.token then
                ( { model | error = Just tokenMissing }, Cmd.none )

            else
                ( { model | discovering = True, error = Nothing, notice = Nothing }
                , GitHub.discover model.token GotDiscovered { repos = model.repos, authors = model.authors }
                )

        GotDiscovered result ->
            case result of
                Err message ->
                    ( { model | discovering = False, error = Just message }, Cmd.none )

                Ok discovery ->
                    let
                        items =
                            Discover.merge discovery.prs model.items
                    in
                    ( { model
                        | items = items
                        , discovering = False
                        , notice = discoveryNotice discovery
                      }
                    , Cmd.batch (storePrs (Persist.encodeItems items) :: badgeCmds items)
                    )

        Tick now ->
            ( { model | now = now }, Cmd.none )


{-| Add the PR parsed from the input: reject invalid URLs, dedupe (highlight
the existing row instead), otherwise prepend, fetch and persist.
-}
add : Model -> ( Model, Cmd Msg )
add model =
    case PrUrl.parse model.urlInput of
        Nothing ->
            ( { model | error = Just "URL de PR invalide." }, Cmd.none )

        Just id ->
            if List.any (\it -> Types.sameId it.id id) model.items then
                ( { model | highlight = Just id, urlInput = "", error = Nothing }, Cmd.none )

            else
                let
                    hasToken =
                        not (String.isEmpty model.token)

                    newItem =
                        { id = id, data = Nothing, fetching = hasToken }

                    items =
                        newItem :: model.items
                in
                ( { model
                    | items = items
                    , urlInput = ""
                    , highlight = Nothing
                    , error =
                        if hasToken then
                            Nothing

                        else
                            Just tokenMissing
                  }
                , Cmd.batch
                    (storePrs (Persist.encodeItems items)
                        :: (if hasToken then
                                [ GitHub.fetchPr model.token GotPr id ]

                            else
                                []
                           )
                    )
                )


{-| Mark every non-finished item as fetching and batch a request for each.
No-op (besides clearing nothing) when there is no token.
-}
refreshAll : Model -> ( List Item, Cmd Msg )
refreshAll model =
    if String.isEmpty model.token then
        ( model.items, Cmd.none )

    else
        ( List.map
            (\it ->
                if Types.isFinished it then
                    it

                else
                    { it | fetching = True }
            )
            model.items
        , model.items
            |> List.filter (not << Types.isFinished)
            |> List.map (\it -> GitHub.fetchPr model.token GotPr it.id)
            |> Cmd.batch
        )


stopFetching : Types.PrId -> List Item -> List Item
stopFetching id =
    List.map
        (\it ->
            if Types.sameId it.id id then
                { it | fetching = False }

            else
                it
        )


{-| Warn at load if there are PRs to refresh but no token to do it with.
-}
loadWarning : Model -> Maybe String
loadWarning model =
    if String.isEmpty model.token && not (List.isEmpty model.items) then
        Just tokenMissing

    else
        Nothing


tokenMissing : String
tokenMissing =
    "Renseigne ton token GitHub pour charger les PR."


repoInvalid : String
repoInvalid =
    "Dépôt invalide (format attendu : owner/repo)."


{-| Feedback after a discovery: how many PRs matched, plus a truncation note
when a search hit its 100-result cap.
-}
discoveryNotice : Types.Discovery -> Maybe String
discoveryNotice discovery =
    let
        count =
            List.length discovery.prs

        base =
            String.fromInt count ++ " PR trouvée(s) par la découverte"
    in
    Just
        (if discovery.truncated then
            base ++ " — certaines non chargées (limite de 100 par recherche atteinte)"

         else
            base
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 60000 Tick
