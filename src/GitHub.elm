module GitHub exposing (decodeDiscovery, discover, fetchPr)

{-| GitHub GraphQL fetches. The browser calls api.github.com directly (CORS is
allowed with an Authorization header), so no backend is needed.

  - `fetchPr` reloads one known PR by `owner/repo/number`.
  - `discover` finds open PRs via search (configured repos × authors, plus PRs
    assigned to the viewer).

-}

import Ci
import Codec
import Discover
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import PrUrl
import Time
import Types exposing (Discovery, PrData, PrId)


endpoint : String
endpoint =
    "https://api.github.com/graphql"


{-| The selection set shared by the single-PR query and the search nodes, so
both decode through the same `Codec.prDataDecoder`.
-}
prFields : String
prFields =
    """
    title
    url
    state
    isDraft
    mergeable
    reviewDecision
    createdAt
    updatedAt
    headRefName
    baseRefName
    author { login avatarUrl }
    reviewThreads(first:100){ nodes { isResolved isOutdated } }
    commits(last:1){ nodes { commit { statusCheckRollup {
      contexts(first:100){ nodes {
        __typename
        ... on CheckRun { conclusion status detailsUrl checkSuite { app { slug } } }
        ... on StatusContext { context state targetUrl }
      } }
    } } } }
    """


query : String
query =
    "query($owner:String!,$repo:String!,$number:Int!){"
        ++ "repository(owner:$owner,name:$repo){pullRequest(number:$number){"
        ++ prFields
        ++ "}}}"


{-| Fetch one PR. The result is a humanised `String` error (ready for the
global banner) or the decoded `PrData`. The originating `PrId` is threaded
back so the caller can match the response to its row.
-}
fetchPr : String -> (PrId -> Result String PrData -> msg) -> PrId -> Cmd msg
fetchPr token toMsg id =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = endpoint
        , body = Http.jsonBody (encodeRequest id)
        , expect = expectGraphQl (toMsg id)
        , timeout = Just 20000
        , tracker = Nothing
        }


encodeRequest : PrId -> Encode.Value
encodeRequest id =
    Encode.object
        [ ( "query", Encode.string query )
        , ( "variables"
          , Encode.object
                [ ( "owner", Encode.string id.owner )
                , ( "repo", Encode.string id.repo )
                , ( "number", Encode.int id.number )
                ]
          )
        ]


{-| Discover open PRs: one search for the configured repos × authors (skipped
unless both lists are non-empty) and one for PRs assigned to the viewer. Both
run as aliased searches in a single request; the result is merged + deduped by
the caller.
-}
discover : String -> (Result String Discovery -> msg) -> { repos : List String, authors : List String, now : Time.Posix } -> Cmd msg
discover token toMsg config =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = endpoint
        , body = Http.jsonBody (encodeSearchRequest (Discover.authorQuery config.now config.repos config.authors))
        , expect = Http.expectStringResponse toMsg (interpret decodeDiscovery)
        , timeout = Just 20000
        , tracker = Nothing
        }


encodeSearchRequest : Maybe String -> Encode.Value
encodeSearchRequest maybeAuthorQuery =
    Encode.object
        [ ( "query", Encode.string (searchQuery (maybeAuthorQuery /= Nothing)) )
        , ( "variables"
          , Encode.object
                (( "assigned", Encode.string Discover.assignedQuery )
                    :: (case maybeAuthorQuery of
                            Just q ->
                                [ ( "byAuthors", Encode.string q ) ]

                            Nothing ->
                                []
                       )
                )
          )
        ]


searchQuery : Bool -> String
searchQuery includeAuthors =
    "query($assigned:String!"
        ++ (if includeAuthors then
                ",$byAuthors:String!"

            else
                ""
           )
        ++ "){"
        ++ searchField "assigned" "$assigned"
        ++ (if includeAuthors then
                searchField "byAuthors" "$byAuthors"

            else
                ""
           )
        ++ "}"


searchField : String -> String -> String
searchField name variable =
    name
        ++ ": search(query:"
        ++ variable
        ++ ",type:ISSUE,first:100){issueCount pageInfo{hasNextPage} nodes{__typename ... on PullRequest{"
        ++ prFields
        ++ "}}}"


{-| Decode a search response into a `Discovery`. Non-PR nodes (and any node
missing PR fields) are dropped rather than failing the whole decode. Exposed
for testing.
-}
decodeDiscovery : String -> Result String Discovery
decodeDiscovery body =
    case Decode.decodeString discoveryResponseDecoder body of
        Ok (Ok disc) ->
            Ok disc

        Ok (Err gqlMsg) ->
            Err gqlMsg

        Err err ->
            Err ("Réponse inattendue : " ++ Decode.errorToString err)


discoveryResponseDecoder : Decoder (Result String Discovery)
discoveryResponseDecoder =
    Decode.oneOf
        [ Decode.field "errors" errorsDecoder |> Decode.map Err
        , Decode.field "data" discoveryDataDecoder |> Decode.map Ok
        ]


discoveryDataDecoder : Decoder Discovery
discoveryDataDecoder =
    Decode.map2
        (\assigned maybeByAuthors ->
            let
                sections =
                    assigned :: List.filterMap identity [ maybeByAuthors ]
            in
            { prs = List.concatMap .prs sections
            , truncated = List.any .truncated sections
            }
        )
        (Decode.field "assigned" searchSectionDecoder)
        (Decode.maybe (Decode.field "byAuthors" searchSectionDecoder))


searchSectionDecoder : Decoder Discovery
searchSectionDecoder =
    Decode.map2 Discovery
        (Decode.field "nodes" (Decode.list (Decode.maybe prDataDecoder) |> Decode.map (List.filterMap identity)))
        (Decode.at [ "pageInfo", "hasNextPage" ] Decode.bool)


{-| Map transport-level outcomes to humanised messages, delegating a 200 body
to the given decoder. Shared by the single-PR and search requests.
-}
interpret : (String -> Result String a) -> Http.Response String -> Result String a
interpret decodeBody response =
    case response of
        Http.BadUrl_ u ->
            Err ("URL invalide : " ++ u)

        Http.Timeout_ ->
            Err "Délai dépassé"

        Http.NetworkError_ ->
            Err "Erreur réseau (connexion ou CORS)"

        Http.BadStatus_ meta _ ->
            case meta.statusCode of
                401 ->
                    Err "Token invalide ou expiré (401)"

                403 ->
                    Err "Accès refusé ou rate limit atteint (403)"

                404 ->
                    Err "PR ou dépôt introuvable (404)"

                code ->
                    Err ("Erreur HTTP " ++ String.fromInt code)

        Http.GoodStatus_ _ body ->
            decodeBody body


{-| GraphQL returns HTTP 200 even for query-level errors (e.g. unknown repo),
which arrive in a top-level `errors` array. We surface both transport errors
(401/403/network) and GraphQL errors as a single humanised message.
-}
expectGraphQl : (Result String PrData -> msg) -> Http.Expect msg
expectGraphQl toMsg =
    Http.expectStringResponse toMsg (interpret decodePrBody)


decodePrBody : String -> Result String PrData
decodePrBody body =
    case Decode.decodeString responseDecoder body of
        Ok (Ok prData) ->
            Ok prData

        Ok (Err gqlMsg) ->
            Err gqlMsg

        Err err ->
            Err ("Réponse inattendue : " ++ Decode.errorToString err)


responseDecoder : Decoder (Result String PrData)
responseDecoder =
    Decode.oneOf
        [ Decode.field "errors" errorsDecoder |> Decode.map Err
        , Decode.at [ "data", "repository", "pullRequest" ] prDataDecoder |> Decode.map Ok
        ]


errorsDecoder : Decoder String
errorsDecoder =
    Decode.list (Decode.field "message" Decode.string)
        |> Decode.map (String.join " ; ")


prDataDecoder : Decoder PrData
prDataDecoder =
    Codec.prDataDecoder idFromVariables unresolvedCountDecoder ciDecoder


{-| Derive the CI status from the head commit's `statusCheckRollup`. Every step
is tolerant: a missing/null rollup, no commit, or no contexts all collapse to
an empty list, i.e. an `unknown` status for both providers.
-}
ciDecoder : Decoder Ci.CiStatus
ciDecoder =
    Decode.oneOf
        [ Decode.at [ "commits", "nodes" ]
            (Decode.index 0
                (Decode.at [ "commit", "statusCheckRollup", "contexts", "nodes" ]
                    (Decode.list rawNodeDecoder)
                )
            )
        , Decode.succeed []
        ]
        |> Decode.map Ci.fromNodes


{-| Decode one rollup context into a `Ci.RawNode`. Every field but the
discriminating `__typename` is optional, so a node never fails to decode
whichever of the `CheckRun`/`StatusContext` shapes it actually is.
-}
rawNodeDecoder : Decoder Ci.RawNode
rawNodeDecoder =
    Decode.map7 Ci.RawNode
        (Decode.field "__typename" Decode.string)
        (Decode.maybe (Decode.at [ "checkSuite", "app", "slug" ] Decode.string))
        (Decode.maybe (Decode.field "conclusion" Decode.string))
        (Decode.maybe (Decode.field "status" Decode.string))
        (Decode.maybe (Decode.field "context" Decode.string))
        (Decode.maybe (Decode.field "state" Decode.string))
        contextUrlDecoder


{-| A `CheckRun` carries a `detailsUrl` (GitHub Actions run); a `StatusContext`
carries a possibly-null `targetUrl` (CircleCI pipeline). We keep whichever is
present as a single link.
-}
contextUrlDecoder : Decoder (Maybe String)
contextUrlDecoder =
    Decode.oneOf
        [ Decode.field "detailsUrl" Decode.string |> Decode.map Just
        , Decode.field "targetUrl" (Decode.nullable Decode.string)
        , Decode.succeed Nothing
        ]


{-| The response object doesn't echo owner/repo/number, so we recover the id
from the URL we know is present in the payload.
-}
idFromVariables : Decoder PrId
idFromVariables =
    Decode.field "url" Decode.string
        |> Decode.andThen
            (\url ->
                case PrUrl.parse url of
                    Just id ->
                        Decode.succeed id

                    Nothing ->
                        Decode.fail ("URL de PR inattendue : " ++ url)
            )


{-| Count review threads that are neither resolved nor outdated — the ones
that still genuinely need attention.
-}
unresolvedCountDecoder : Decoder Int
unresolvedCountDecoder =
    Decode.at [ "reviewThreads", "nodes" ] (Decode.list threadDecoder)
        |> Decode.map (List.filter identity >> List.length)


threadDecoder : Decoder Bool
threadDecoder =
    Decode.map2 (\resolved outdated -> not resolved && not outdated)
        (Decode.field "isResolved" Decode.bool)
        (Decode.field "isOutdated" Decode.bool)
