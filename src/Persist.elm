module Persist exposing (Flags, decodeFlags, encodeItems)

{-| Browser persistence. Open PRs store only their id (refetched on load);
finished PRs store a frozen snapshot. Decoding is lenient: a malformed entry
is dropped rather than wiping the whole list.
-}

import Ci
import Codec
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time
import Types exposing (Item, PrData)


type alias Flags =
    { token : String
    , items : List Item
    , repos : List String
    , authors : List String
    , now : Time.Posix
    }


decodeFlags : Decode.Value -> Flags
decodeFlags value =
    { token = decodeOr (Decode.field "token" Decode.string) "" value
    , items =
        decodeOr
            (Decode.field "prs" (Decode.list (Decode.maybe storedItemDecoder)))
            []
            value
            |> List.filterMap identity
    , repos = decodeOr (Decode.field "repos" (Decode.list Decode.string)) [] value
    , authors = decodeOr (Decode.field "authors" (Decode.list Decode.string)) [] value
    , now = Time.millisToPosix (decodeOr (Decode.field "now" Decode.int) 0 value)
    }


decodeOr : Decoder a -> a -> Decode.Value -> a
decodeOr decoder default value =
    Decode.decodeValue decoder value |> Result.withDefault default


encodeItems : List Item -> Encode.Value
encodeItems items =
    Encode.list encodeItem items


encodeItem : Item -> Encode.Value
encodeItem item =
    case ( Types.isFinished item, item.data ) of
        ( True, Just d ) ->
            Encode.object
                [ ( "kind", Encode.string "frozen" )
                , ( "data", Codec.encodePrData d )
                ]

        _ ->
            Encode.object
                [ ( "kind", Encode.string "open" )
                , ( "id", Codec.encodePrId item.id )
                ]


storedItemDecoder : Decoder Item
storedItemDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "frozen" ->
                        Decode.field "data" frozenDataDecoder
                            |> Decode.map (\d -> Item d.id (Just d) False)

                    _ ->
                        Decode.field "id" Codec.prIdDecoder
                            |> Decode.map (\id -> Item id Nothing False)
            )


frozenDataDecoder : Decoder PrData
frozenDataDecoder =
    Codec.prDataDecoder
        (Decode.field "id" Codec.prIdDecoder)
        (Decode.field "unresolvedCount" Decode.int)
        (Decode.succeed Ci.unknownStatus)
