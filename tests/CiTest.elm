module CiTest exposing (suite)

import Ci exposing (CiState(..), Classified(..))
import Expect
import Test exposing (Test, describe, test)


{-| A bare raw node; each test overrides only the fields it cares about.
-}
node : Ci.RawNode
node =
    { typename = "CheckRun"
    , appSlug = Nothing
    , conclusion = Nothing
    , checkStatus = Nothing
    , context = Nothing
    , state = Nothing
    , url = Nothing
    }


gha : String -> String -> Ci.RawNode
gha status conclusion =
    { node
        | typename = "CheckRun"
        , appSlug = Just "github-actions"
        , checkStatus = Just status
        , conclusion = Just conclusion
        , url = Just "https://github.com/o/r/actions/runs/1"
    }


circle : String -> Ci.RawNode
circle state =
    { node
        | typename = "StatusContext"
        , context = Just "ci/circleci: build"
        , state = Just state
        , url = Just "https://app.circleci.com/pipelines/1"
    }


suite : Test
suite =
    describe "Ci"
        [ describe "ghaState mapping"
            [ test "completed success -> Succeed" <|
                \_ -> Ci.ghaState (Just "SUCCESS") (Just "COMPLETED") |> Expect.equal (Just Succeed)
            , test "completed failure -> Failed" <|
                \_ -> Ci.ghaState (Just "FAILURE") (Just "COMPLETED") |> Expect.equal (Just Failed)
            , test "completed timed_out -> Failed" <|
                \_ -> Ci.ghaState (Just "TIMED_OUT") (Just "COMPLETED") |> Expect.equal (Just Failed)
            , test "in_progress -> InProgress regardless of conclusion" <|
                \_ -> Ci.ghaState Nothing (Just "IN_PROGRESS") |> Expect.equal (Just InProgress)
            , test "queued -> InProgress" <|
                \_ -> Ci.ghaState Nothing (Just "QUEUED") |> Expect.equal (Just InProgress)
            , test "skipped -> dropped (Nothing)" <|
                \_ -> Ci.ghaState (Just "SKIPPED") (Just "COMPLETED") |> Expect.equal Nothing
            , test "neutral -> dropped" <|
                \_ -> Ci.ghaState (Just "NEUTRAL") (Just "COMPLETED") |> Expect.equal Nothing
            , test "cancelled -> dropped" <|
                \_ -> Ci.ghaState (Just "CANCELLED") (Just "COMPLETED") |> Expect.equal Nothing
            ]
        , describe "circleState mapping"
            [ test "SUCCESS -> Succeed" <|
                \_ -> Ci.circleState (Just "SUCCESS") |> Expect.equal (Just Succeed)
            , test "FAILURE -> Failed" <|
                \_ -> Ci.circleState (Just "FAILURE") |> Expect.equal (Just Failed)
            , test "ERROR -> Failed" <|
                \_ -> Ci.circleState (Just "ERROR") |> Expect.equal (Just Failed)
            , test "PENDING -> InProgress" <|
                \_ -> Ci.circleState (Just "PENDING") |> Expect.equal (Just InProgress)
            , test "EXPECTED -> InProgress" <|
                \_ -> Ci.circleState (Just "EXPECTED") |> Expect.equal (Just InProgress)
            ]
        , describe "classify (strict)"
            [ test "github-actions CheckRun -> AsGha" <|
                \_ ->
                    Ci.classify (gha "COMPLETED" "SUCCESS")
                        |> Expect.equal (AsGha { state = Succeed, url = Just "https://github.com/o/r/actions/runs/1" })
            , test "ci/circleci StatusContext -> AsCircle with pipeline url" <|
                \_ ->
                    Ci.classify (circle "SUCCESS")
                        |> Expect.equal (AsCircle { state = Succeed, url = Just "https://app.circleci.com/pipelines/1" })
            , test "CheckRun from another app is skipped" <|
                \_ ->
                    Ci.classify { node | typename = "CheckRun", appSlug = Just "other-bot", checkStatus = Just "COMPLETED", conclusion = Just "FAILURE" }
                        |> Expect.equal Skip
            , test "non-circleci StatusContext (sonarcloud) is skipped" <|
                \_ ->
                    Ci.classify { node | typename = "StatusContext", context = Just "SonarCloud Code Analysis", state = Just "FAILURE" }
                        |> Expect.equal Skip
            , test "skipped gha check classifies to Skip" <|
                \_ -> Ci.classify (gha "COMPLETED" "SKIPPED") |> Expect.equal Skip
            ]
        , describe "aggregate priority"
            [ test "any failed wins, keeps first failed url" <|
                \_ ->
                    Ci.aggregate
                        [ { state = Succeed, url = Just "ok" }
                        , { state = Failed, url = Just "boom" }
                        , { state = InProgress, url = Just "run" }
                        ]
                        |> Expect.equal { state = Failed, url = Just "boom" }
            , test "in-progress beats succeed when no failure" <|
                \_ ->
                    Ci.aggregate
                        [ { state = Succeed, url = Just "ok" }
                        , { state = InProgress, url = Just "run" }
                        ]
                        |> Expect.equal { state = InProgress, url = Just "run" }
            , test "all succeed -> Succeed" <|
                \_ ->
                    Ci.aggregate [ { state = Succeed, url = Just "a" }, { state = Succeed, url = Just "b" } ]
                        |> Expect.equal { state = Succeed, url = Just "a" }
            , test "empty -> Unknown with no url" <|
                \_ -> Ci.aggregate [] |> Expect.equal { state = Unknown, url = Nothing }
            ]
        , describe "fromNodes integration"
            [ test "mixes providers, ignores noise, aggregates each" <|
                \_ ->
                    let
                        nodes =
                            [ gha "COMPLETED" "SUCCESS"
                            , { node | typename = "CheckRun", appSlug = Just "github-actions", checkStatus = Just "COMPLETED", conclusion = Just "FAILURE", url = Just "ghafail" }
                            , circle "SUCCESS"
                            , { node | typename = "StatusContext", context = Just "codecov/project", state = Just "FAILURE" }
                            ]

                        result =
                            Ci.fromNodes nodes
                    in
                    Expect.all
                        [ \r -> Expect.equal Failed r.gha.state
                        , \r -> Expect.equal (Just "ghafail") r.gha.url
                        , \r -> Expect.equal Succeed r.circle.state
                        , \r -> Expect.equal (Just "https://app.circleci.com/pipelines/1") r.circle.url
                        ]
                        result
            , test "no checks at all -> both Unknown" <|
                \_ -> Ci.fromNodes [] |> Expect.equal Ci.unknownStatus
            ]
        ]
