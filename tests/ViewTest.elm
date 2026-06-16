module ViewTest exposing (configSuite, discoverSuite, suite)

import Ci
import Expect
import Model exposing (Model)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Types exposing (Mergeable(..), PrData, PrState(..), ReviewDecision(..))
import View


baseModel : Model
baseModel =
    { token = ""
    , urlInput = ""
    , items = []
    , repos = []
    , authors = []
    , repoInput = ""
    , authorInput = ""
    , configOpen = True
    , discovering = False
    , now = Time.millisToPosix 0
    , error = Nothing
    , notice = Nothing
    , highlight = Nothing
    , lastRefresh = Nothing
    }


basePr : PrData
basePr =
    { id = { owner = "o", repo = "r", number = 1 }
    , title = "Some PR"
    , url = "https://example.com"
    , author = { login = "me", avatarUrl = "" }
    , headRef = "feature"
    , baseRef = "main"
    , reviewDecision = Approved
    , unresolvedCount = 0
    , mergeable = Mergeable
    , state = StOpen
    , createdAt = Time.millisToPosix 0
    , updatedAt = Time.millisToPosix 0
    , ci = Ci.unknownStatus
    }


withGha : Ci.CiState -> PrData -> PrData
withGha state pr =
    { pr | ci = { gha = { state = state, url = Nothing }, circle = { state = Ci.Unknown, url = Nothing } } }


configSuite : Test
configSuite =
    describe "View.viewConfig"
        [ test "renders a chip for each configured repo" <|
            \_ ->
                View.viewConfig { baseModel | repos = [ "o/r1", "o/r2" ] }
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "chip" ]
                    |> Query.count (Expect.equal 2)
        , test "renders the author login as a chip" <|
            \_ ->
                View.viewConfig { baseModel | authors = [ "octocat" ] }
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "chip", Selector.text "octocat" ]
        , test "hides the config body when collapsed" <|
            \_ ->
                View.viewConfig { baseModel | configOpen = False, repos = [ "o/r" ] }
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "config-body" ]
                    |> Query.count (Expect.equal 0)
        ]


discoverSuite : Test
discoverSuite =
    describe "View discovery controls"
        [ test "shows the discover button by default" <|
            \_ ->
                View.view baseModel
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Récupérer les nouvelles PR" ]
        , test "labels the button while discovering" <|
            \_ ->
                View.view { baseModel | discovering = True }
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Récupération…" ]
        , test "renders an info banner for a notice" <|
            \_ ->
                View.view { baseModel | notice = Just "3 PR trouvée(s)" }
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "banner-info", Selector.text "3 PR trouvée(s)" ]
        ]


suite : Test
suite =
    describe "View.viewBadges"
        [ test "status badges sit on a first row" <|
            \_ ->
                View.viewBadges basePr
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "badge-row" ]
        , test "no CI row when both providers are Unknown (collapse)" <|
            \_ ->
                View.viewBadges basePr
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "badge-row-ci" ]
                    |> Query.count (Expect.equal 0)
        , test "a CI row appears when a provider reports a state" <|
            \_ ->
                basePr
                    |> withGha Ci.Succeed
                    |> View.viewBadges
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "badge-row-ci" ]
        , test "the reporting provider's badge is rendered" <|
            \_ ->
                basePr
                    |> withGha Ci.Succeed
                    |> View.viewBadges
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "✓ GHA" ]
        , test "finished PRs never show a CI row" <|
            \_ ->
                { basePr | state = StMerged }
                    |> withGha Ci.Succeed
                    |> View.viewBadges
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "badge-row-ci" ]
                    |> Query.count (Expect.equal 0)
        ]
