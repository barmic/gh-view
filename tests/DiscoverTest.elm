module DiscoverTest exposing (suite)

import Ci
import Discover
import Expect
import Test exposing (Test, describe, test)
import Time
import Types exposing (Item, Mergeable(..), PrData, PrState(..), ReviewDecision(..))


pr : String -> String -> Int -> PrData
pr owner repo number =
    { id = { owner = owner, repo = repo, number = number }
    , title = owner ++ "/" ++ repo ++ " #" ++ String.fromInt number
    , url = "https://github.com/" ++ owner ++ "/" ++ repo ++ "/pull/" ++ String.fromInt number
    , author = { login = "alice", avatarUrl = "" }
    , headRef = "feature"
    , baseRef = "main"
    , reviewDecision = NoReview
    , unresolvedCount = 0
    , mergeable = UnknownMergeable
    , state = StOpen
    , createdAt = Time.millisToPosix 0
    , updatedAt = Time.millisToPosix 0
    , ci = Ci.unknownStatus
    }


item : String -> String -> Int -> Maybe PrData -> Item
item owner repo number data =
    { id = { owner = owner, repo = repo, number = number }, data = data, fetching = False }


{-| 2026-06-17T00:00:00Z. The 31-day window then starts at 2026-05-17T00:00:00Z.
-}
now : Time.Posix
now =
    Time.millisToPosix 1781654400000


suite : Test
suite =
    describe "Discover"
        [ describe "authorQuery"
            [ test "is Nothing when no repos are configured" <|
                \_ -> Discover.authorQuery now [] [ "alice" ] |> Expect.equal Nothing
            , test "is Nothing when no authors are configured" <|
                \_ -> Discover.authorQuery now [ "o/r" ] [] |> Expect.equal Nothing
            , test "combines repos and authors and excludes drafts" <|
                \_ ->
                    Discover.authorQuery now [ "o/r1", "o/r2" ] [ "alice", "bob" ]
                        |> Expect.equal (Just "is:open is:pr -is:draft repo:o/r1 repo:o/r2 author:alice author:bob updated:>=2026-05-17T00:00:00.000Z")
            ]
        , describe "assignedQuery"
            [ test "excludes drafts" <|
                \_ -> Discover.assignedQuery |> Expect.equal "is:open is:pr -is:draft assignee:@me"
            , test "is not limited in time (no updated qualifier)" <|
                \_ -> Discover.assignedQuery |> String.contains "updated:" |> Expect.equal False
            ]
        , describe "merge"
            [ test "appends a discovered PR not yet in the list" <|
                \_ ->
                    Discover.merge [ pr "o" "r" 1 ] []
                        |> List.map .id
                        |> Expect.equal [ { owner = "o", repo = "r", number = 1 } ]
            , test "refreshes the data of an item already present" <|
                \_ ->
                    Discover.merge [ pr "o" "r" 1 ] [ item "o" "r" 1 Nothing ]
                        |> List.map (\it -> ( it.id.number, it.data /= Nothing ))
                        |> Expect.equal [ ( 1, True ) ]
            , test "does not duplicate an item already present" <|
                \_ ->
                    Discover.merge [ pr "o" "r" 1 ] [ item "o" "r" 1 Nothing ]
                        |> List.length
                        |> Expect.equal 1
            , test "keeps manual items that were not rediscovered" <|
                \_ ->
                    Discover.merge [ pr "o" "r" 2 ] [ item "o" "r" 1 Nothing ]
                        |> List.map (.id >> .number)
                        |> Expect.equal [ 1, 2 ]
            , test "drops duplicates among the discovered PRs themselves" <|
                \_ ->
                    Discover.merge [ pr "o" "r" 1, pr "o" "r" 1 ] []
                        |> List.length
                        |> Expect.equal 1
            ]
        ]
