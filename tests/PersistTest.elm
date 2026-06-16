module PersistTest exposing (suite)

import Expect
import Json.Encode as Encode
import Persist
import Test exposing (Test, describe, test)


flagsWith : List ( String, Encode.Value ) -> Encode.Value
flagsWith fields =
    Encode.object fields


suite : Test
suite =
    describe "Persist.decodeFlags"
        [ test "reads the configured repos" <|
            \_ ->
                flagsWith [ ( "repos", Encode.list Encode.string [ "o/r1", "o/r2" ] ) ]
                    |> Persist.decodeFlags
                    |> .repos
                    |> Expect.equal [ "o/r1", "o/r2" ]
        , test "reads the configured authors" <|
            \_ ->
                flagsWith [ ( "authors", Encode.list Encode.string [ "alice", "bob" ] ) ]
                    |> Persist.decodeFlags
                    |> .authors
                    |> Expect.equal [ "alice", "bob" ]
        , test "defaults repos to empty when absent" <|
            \_ ->
                flagsWith []
                    |> Persist.decodeFlags
                    |> .repos
                    |> Expect.equal []
        , test "defaults authors to empty when absent" <|
            \_ ->
                flagsWith []
                    |> Persist.decodeFlags
                    |> .authors
                    |> Expect.equal []
        ]
