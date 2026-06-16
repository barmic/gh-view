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


suite : Test
suite =
    describe "Discover"
        [ describe "authorQuery"
            [ test "is Nothing when no repos are configured" <|
                \_ -> Discover.authorQuery [] [ "alice" ] |> Expect.equal Nothing
            , test "is Nothing when no authors are configured" <|
                \_ -> Discover.authorQuery [ "o/r" ] [] |> Expect.equal Nothing
            , test "combines repos and authors into a search query" <|
                \_ ->
                    Discover.authorQuery [ "o/r1", "o/r2" ] [ "alice", "bob" ]
                        |> Expect.equal (Just "is:open is:pr repo:o/r1 repo:o/r2 author:alice author:bob")
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
