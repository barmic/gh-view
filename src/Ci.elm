module Ci exposing
    ( Check
    , CiState(..)
    , CiStatus
    , Classified(..)
    , ProviderCi
    , RawNode
    , aggregate
    , circleState
    , classify
    , fromNodes
    , ghaState
    , unknownStatus
    )

{-| Continuous-integration status, derived from GitHub's `statusCheckRollup`.

A single GitHub GraphQL query exposes every check on a PR's head commit, both
GitHub Actions (as `CheckRun`) and CircleCI (as legacy `StatusContext`). This
module turns that raw, mixed list into two aggregated per-provider states.

Everything here is pure: the decoder feeds in already-extracted `RawNode`s and
this module classifies, maps and aggregates them, which keeps the business
rules (strict provider classification, state mapping, priority aggregation)
testable without any HTTP or JSON.

-}


{-| The four states a provider's CI can be in, for one PR.

`Unknown` means the provider reported no check at all (e.g. a repo without
CircleCI). It is never produced by a single check, only by aggregation over an
empty set.

-}
type CiState
    = Unknown
    | InProgress
    | Failed
    | Succeed


{-| One provider's aggregated CI, with an optional link (the head check's
`detailsUrl` for GitHub Actions, the `targetUrl` pipeline link for CircleCI).
-}
type alias ProviderCi =
    { state : CiState
    , url : Maybe String
    }


{-| The two providers we surface, side by side.
-}
type alias CiStatus =
    { gha : ProviderCi
    , circle : ProviderCi
    }


{-| A single meaningful check, after mapping. Its `state` is never `Unknown`:
neutral/skipped/cancelled checks are dropped before this point.
-}
type alias Check =
    { state : CiState
    , url : Maybe String
    }


{-| One raw context node, as extracted by the decoder from `statusCheckRollup`.

`url` already holds the right link per type (a `CheckRun`'s `detailsUrl` or a
`StatusContext`'s `targetUrl`), so classification never has to choose.

-}
type alias RawNode =
    { typename : String
    , appSlug : Maybe String
    , conclusion : Maybe String
    , checkStatus : Maybe String
    , context : Maybe String
    , state : Maybe String
    , url : Maybe String
    }


{-| Outcome of strict provider classification for one node.
-}
type Classified
    = AsGha Check
    | AsCircle Check
    | Skip


unknownStatus : CiStatus
unknownStatus =
    { gha = { state = Unknown, url = Nothing }
    , circle = { state = Unknown, url = Nothing }
    }


{-| Map a GitHub Actions `CheckRun` (its `conclusion` and `status`) to a check
state, or `Nothing` for neutral/skipped/cancelled checks that must not count.
-}
ghaState : Maybe String -> Maybe String -> Maybe CiState
ghaState conclusion checkStatus =
    if checkStatus == Just "COMPLETED" then
        case conclusion of
            Just "SUCCESS" ->
                Just Succeed

            Just "FAILURE" ->
                Just Failed

            Just "TIMED_OUT" ->
                Just Failed

            Just "STARTUP_FAILURE" ->
                Just Failed

            Just "ACTION_REQUIRED" ->
                Just Failed

            _ ->
                -- NEUTRAL / SKIPPED / CANCELLED / unknown: do not count
                Nothing

    else
        -- QUEUED / IN_PROGRESS / WAITING / PENDING / …
        Just InProgress


{-| Map a CircleCI `StatusContext` `state` to a check state, or `Nothing` for
states that must not count.
-}
circleState : Maybe String -> Maybe CiState
circleState state =
    case state of
        Just "SUCCESS" ->
            Just Succeed

        Just "FAILURE" ->
            Just Failed

        Just "ERROR" ->
            Just Failed

        Just "PENDING" ->
            Just InProgress

        Just "EXPECTED" ->
            Just InProgress

        _ ->
            Nothing


{-| Strict classification: a node is GitHub Actions only when it is a
`CheckRun` from the `github-actions` app, and CircleCI only when it is a
`StatusContext` whose context is a `ci/circleci…` one. Everything else
(SonarCloud, Codecov, Mergify, …) is skipped.
-}
classify : RawNode -> Classified
classify n =
    if n.typename == "CheckRun" && n.appSlug == Just "github-actions" then
        toClassified AsGha n (ghaState n.conclusion n.checkStatus)

    else if n.typename == "StatusContext" && isCircleContext n.context then
        toClassified AsCircle n (circleState n.state)

    else
        Skip


isCircleContext : Maybe String -> Bool
isCircleContext context =
    case context of
        Just c ->
            String.startsWith "ci/circleci" c

        Nothing ->
            False


toClassified : (Check -> Classified) -> RawNode -> Maybe CiState -> Classified
toClassified wrap n maybeState =
    case maybeState of
        Just state ->
            wrap { state = state, url = n.url }

        Nothing ->
            Skip


{-| Aggregate one provider's checks with the priority
failed > in-progress > succeed > unknown, keeping the link of the winning
group's first check.
-}
aggregate : List Check -> ProviderCi
aggregate checks =
    case pick Failed checks of
        Just c ->
            c

        Nothing ->
            case pick InProgress checks of
                Just c ->
                    c

                Nothing ->
                    case pick Succeed checks of
                        Just c ->
                            c

                        Nothing ->
                            { state = Unknown, url = Nothing }


{-| The first check in `state`, as a `ProviderCi` carrying its link.
-}
pick : CiState -> List Check -> Maybe ProviderCi
pick state checks =
    checks
        |> List.filter (\c -> c.state == state)
        |> List.head
        |> Maybe.map (\c -> { state = c.state, url = c.url })


{-| Full pipeline: classify every node, then aggregate per provider.
-}
fromNodes : List RawNode -> CiStatus
fromNodes nodes =
    let
        classified =
            List.map classify nodes

        ghaChecks =
            List.filterMap onlyGha classified

        circleChecks =
            List.filterMap onlyCircle classified
    in
    { gha = aggregate ghaChecks
    , circle = aggregate circleChecks
    }


onlyGha : Classified -> Maybe Check
onlyGha c =
    case c of
        AsGha check ->
            Just check

        _ ->
            Nothing


onlyCircle : Classified -> Maybe Check
onlyCircle c =
    case c of
        AsCircle check ->
            Just check

        _ ->
            Nothing
