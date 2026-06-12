module Codec exposing
    ( encodePrData
    , encodePrId
    , prDataDecoder
    , prIdDecoder
    )

{-| Shared (de)serialization for `PrData`, used both for GitHub GraphQL
responses and for `localStorage` snapshots.

The two sources differ in exactly two places, which are passed in:

  - how the `PrId` is obtained (injected for GitHub, read from JSON for storage)
  - how the unresolved-comment count is obtained (computed from
    `reviewThreads` for GitHub, stored as an int for storage)
  - how the CI status is obtained (derived from `statusCheckRollup` for GitHub,
    defaulted to `unknown` for storage — CI is intentionally not persisted)

Everything else (enums as GitHub's own string forms, ISO-8601 dates) is
encoded identically, so a frozen snapshot round-trips through the same field
names the API uses.

-}

import Ci
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time
import Types exposing (Author, Mergeable(..), PrData, PrId, PrState(..), ReviewDecision(..))


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


{-| Decode a `PrData`, parameterised over its id source and its
unresolved-count source. Field order must match the `PrData` constructor.
-}
prDataDecoder : Decoder PrId -> Decoder Int -> Decoder Ci.CiStatus -> Decoder PrData
prDataDecoder idDecoder countDecoder ciDecoder =
    Decode.succeed PrData
        |> andMap idDecoder
        |> andMap (Decode.field "title" Decode.string)
        |> andMap (Decode.field "url" Decode.string)
        |> andMap authorField
        |> andMap (Decode.field "headRefName" Decode.string)
        |> andMap (Decode.field "baseRefName" Decode.string)
        |> andMap reviewDecisionField
        |> andMap countDecoder
        |> andMap mergeableField
        |> andMap stateField
        |> andMap (Decode.field "createdAt" Iso8601.decoder)
        |> andMap (Decode.field "updatedAt" Iso8601.decoder)
        |> andMap ciDecoder


encodePrData : PrData -> Encode.Value
encodePrData d =
    Encode.object
        [ ( "id", encodePrId d.id )
        , ( "title", Encode.string d.title )
        , ( "url", Encode.string d.url )
        , ( "author"
          , Encode.object
                [ ( "login", Encode.string d.author.login )
                , ( "avatarUrl", Encode.string d.author.avatarUrl )
                ]
          )
        , ( "headRefName", Encode.string d.headRef )
        , ( "baseRefName", Encode.string d.baseRef )
        , ( "reviewDecision", encodeReviewDecision d.reviewDecision )
        , ( "unresolvedCount", Encode.int d.unresolvedCount )
        , ( "mergeable", Encode.string (mergeableToString d.mergeable) )
        , ( "state", Encode.string (stateToString d.state) )
        , ( "createdAt", Iso8601.encode d.createdAt )
        , ( "updatedAt", Iso8601.encode d.updatedAt )
        ]



-- PrId


prIdDecoder : Decoder PrId
prIdDecoder =
    Decode.map3 PrId
        (Decode.field "owner" Decode.string)
        (Decode.field "repo" Decode.string)
        (Decode.field "number" Decode.int)


encodePrId : PrId -> Encode.Value
encodePrId id =
    Encode.object
        [ ( "owner", Encode.string id.owner )
        , ( "repo", Encode.string id.repo )
        , ( "number", Encode.int id.number )
        ]



-- Author (may be null for a deleted account)


authorField : Decoder Author
authorField =
    Decode.field "author" (Decode.nullable authorDecoder)
        |> Decode.map (Maybe.withDefault { login = "ghost", avatarUrl = "" })


authorDecoder : Decoder Author
authorDecoder =
    Decode.map2 Author
        (Decode.field "login" Decode.string)
        (Decode.field "avatarUrl" Decode.string)



-- Enums, encoded as GitHub's own string forms


reviewDecisionField : Decoder ReviewDecision
reviewDecisionField =
    Decode.field "reviewDecision" (Decode.nullable Decode.string)
        |> Decode.map
            (\m ->
                case m of
                    Just "APPROVED" ->
                        Approved

                    Just "CHANGES_REQUESTED" ->
                        ChangesRequested

                    Just "REVIEW_REQUIRED" ->
                        ReviewRequired

                    _ ->
                        NoReview
            )


encodeReviewDecision : ReviewDecision -> Encode.Value
encodeReviewDecision rd =
    case rd of
        Approved ->
            Encode.string "APPROVED"

        ChangesRequested ->
            Encode.string "CHANGES_REQUESTED"

        ReviewRequired ->
            Encode.string "REVIEW_REQUIRED"

        NoReview ->
            Encode.null


mergeableField : Decoder Mergeable
mergeableField =
    Decode.field "mergeable" Decode.string
        |> Decode.map
            (\s ->
                case s of
                    "MERGEABLE" ->
                        Mergeable

                    "CONFLICTING" ->
                        Conflicting

                    _ ->
                        UnknownMergeable
            )


mergeableToString : Mergeable -> String
mergeableToString m =
    case m of
        Mergeable ->
            "MERGEABLE"

        Conflicting ->
            "CONFLICTING"

        UnknownMergeable ->
            "UNKNOWN"


stateField : Decoder PrState
stateField =
    Decode.field "state" Decode.string
        |> Decode.map
            (\s ->
                case s of
                    "MERGED" ->
                        StMerged

                    "CLOSED" ->
                        StClosed

                    _ ->
                        StOpen
            )


stateToString : PrState -> String
stateToString s =
    case s of
        StOpen ->
            "OPEN"

        StClosed ->
            "CLOSED"

        StMerged ->
            "MERGED"
