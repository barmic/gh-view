module ConfigTest exposing (suite)

import Config
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Config"
        [ describe "parseRepo"
            [ test "accepts a plain owner/repo slug" <|
                \_ -> Config.parseRepo "gravitee-io/apim" |> Expect.equal (Just "gravitee-io/apim")
            , test "trims surrounding whitespace" <|
                \_ -> Config.parseRepo "  owner/repo  " |> Expect.equal (Just "owner/repo")
            , test "rejects a value without a slash" <|
                \_ -> Config.parseRepo "notaslug" |> Expect.equal Nothing
            , test "rejects more than two segments" <|
                \_ -> Config.parseRepo "owner/repo/extra" |> Expect.equal Nothing
            , test "rejects an empty value" <|
                \_ -> Config.parseRepo "" |> Expect.equal Nothing
            , test "rejects an empty segment" <|
                \_ -> Config.parseRepo "owner/" |> Expect.equal Nothing
            ]
        , describe "normalizeAuthor"
            [ test "strips a leading @" <|
                \_ -> Config.normalizeAuthor "@octocat" |> Expect.equal "octocat"
            , test "leaves a bare login untouched" <|
                \_ -> Config.normalizeAuthor "octocat" |> Expect.equal "octocat"
            , test "trims and strips @" <|
                \_ -> Config.normalizeAuthor "  @octocat " |> Expect.equal "octocat"
            ]
        , describe "addItem"
            [ test "appends a new item to the end" <|
                \_ -> Config.addItem "b" [ "a" ] |> Expect.equal [ "a", "b" ]
            , test "does not add a case-insensitive duplicate" <|
                \_ -> Config.addItem "Octocat" [ "octocat" ] |> Expect.equal [ "octocat" ]
            ]
        , describe "removeItem"
            [ test "removes the matching item" <|
                \_ -> Config.removeItem "a" [ "a", "b" ] |> Expect.equal [ "b" ]
            , test "removes case-insensitively" <|
                \_ -> Config.removeItem "Octocat" [ "octocat", "b" ] |> Expect.equal [ "b" ]
            ]
        ]
