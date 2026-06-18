module GitHubTest exposing (suite)

import Expect
import GitHub
import Test exposing (Test, describe, test)
import Types exposing (PrState(..))


prNode : String -> Int -> String
prNode repo number =
    """
    { "__typename": "PullRequest"
    , "title": "A PR"
    , "url": "https://github.com/o/__REPO__/pull/__N__"
    , "state": "OPEN"
    , "mergeable": "MERGEABLE"
    , "reviewDecision": null
    , "createdAt": "2024-01-01T00:00:00Z"
    , "updatedAt": "2024-01-02T00:00:00Z"
    , "headRefName": "feature"
    , "baseRefName": "main"
    , "author": { "login": "alice", "avatarUrl": "" }
    , "reviewThreads": { "nodes": [] }
    , "commits": { "nodes": [] }
    }
    """
        |> String.replace "__REPO__" repo
        |> String.replace "__N__" (String.fromInt number)


prNodeState : String -> Bool -> String
prNodeState state isDraft =
    """
    { "__typename": "PullRequest"
    , "title": "A PR"
    , "url": "https://github.com/o/r/pull/1"
    , "state": "__STATE__"
    , "isDraft": __DRAFT__
    , "mergeable": "MERGEABLE"
    , "reviewDecision": null
    , "createdAt": "2024-01-01T00:00:00Z"
    , "updatedAt": "2024-01-02T00:00:00Z"
    , "headRefName": "feature"
    , "baseRefName": "main"
    , "author": { "login": "alice", "avatarUrl": "" }
    , "reviewThreads": { "nodes": [] }
    , "commits": { "nodes": [] }
    }
    """
        |> String.replace "__STATE__" state
        |> String.replace "__DRAFT__"
            (if isDraft then
                "true"

             else
                "false"
            )


decodeStates : String -> Bool -> Result String (List PrState)
decodeStates state isDraft =
    ("""{ "data": { "assigned": """ ++ section False [ prNodeState state isDraft ] ++ "}}")
        |> GitHub.decodeDiscovery
        |> Result.map (.prs >> List.map .state)


section : Bool -> List String -> String
section hasNext nodes =
    """{ "issueCount": 0, "pageInfo": { "hasNextPage": __NEXT__ }, "nodes": [ __NODES__ ] }"""
        |> String.replace "__NEXT__"
            (if hasNext then
                "true"

             else
                "false"
            )
        |> String.replace "__NODES__" (String.join "," nodes)


suite : Test
suite =
    describe "GitHub.decodeDiscovery"
        [ test "collects PRs from both search sections" <|
            \_ ->
                ("""{ "data": { "assigned": """
                    ++ section False [ prNode "r" 5 ]
                    ++ ""","byAuthors": """
                    ++ section False [ prNode "r" 7 ]
                    ++ "}}"
                )
                    |> GitHub.decodeDiscovery
                    |> Result.map (.prs >> List.map (.id >> .number))
                    |> Expect.equal (Ok [ 5, 7 ])
        , test "drops non-PR nodes instead of failing" <|
            \_ ->
                ("""{ "data": { "assigned": """
                    ++ section False [ prNode "r" 5, """{ "__typename": "Issue" }""" ]
                    ++ "}}"
                )
                    |> GitHub.decodeDiscovery
                    |> Result.map (.prs >> List.length)
                    |> Expect.equal (Ok 1)
        , test "works when only the assigned section is present" <|
            \_ ->
                ("""{ "data": { "assigned": """ ++ section False [ prNode "r" 5 ] ++ "}}")
                    |> GitHub.decodeDiscovery
                    |> Result.map (.prs >> List.length)
                    |> Expect.equal (Ok 1)
        , test "reports truncation when any section has a next page" <|
            \_ ->
                ("""{ "data": { "assigned": """
                    ++ section True [ prNode "r" 5 ]
                    ++ "}}"
                )
                    |> GitHub.decodeDiscovery
                    |> Result.map .truncated
                    |> Expect.equal (Ok True)
        , test "surfaces a GraphQL error message" <|
            \_ ->
                """{ "errors": [ { "message": "Something went wrong" } ] }"""
                    |> GitHub.decodeDiscovery
                    |> Expect.equal (Err "Something went wrong")
        , test "an open draft decodes to StDraft" <|
            \_ ->
                decodeStates "OPEN" True
                    |> Expect.equal (Ok [ StDraft ])
        , test "a non-draft open PR stays StOpen" <|
            \_ ->
                decodeStates "OPEN" False
                    |> Expect.equal (Ok [ StOpen ])
        , test "a closed draft is StClosed (terminal state wins)" <|
            \_ ->
                decodeStates "CLOSED" True
                    |> Expect.equal (Ok [ StClosed ])
        ]
