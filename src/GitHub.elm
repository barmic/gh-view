module GitHub exposing (fetchPr)

{-| Single-PR GitHub GraphQL fetch. The browser calls api.github.com directly
(CORS is allowed with an Authorization header), so no backend is needed.
-}

import Codec
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import PrUrl
import Types exposing (PrData, PrId)


endpoint : String
endpoint =
    "https://api.github.com/graphql"


query : String
query =
    """
    query($owner:String!,$repo:String!,$number:Int!){
      repository(owner:$owner,name:$repo){
        pullRequest(number:$number){
          title
          url
          state
          mergeable
          reviewDecision
          createdAt
          updatedAt
          headRefName
          baseRefName
          author { login avatarUrl }
          reviewThreads(first:100){ nodes { isResolved isOutdated } }
        }
      }
    }
    """


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


{-| GraphQL returns HTTP 200 even for query-level errors (e.g. unknown repo),
which arrive in a top-level `errors` array. We surface both transport errors
(401/403/network) and GraphQL errors as a single humanised message.
-}
expectGraphQl : (Result String PrData -> msg) -> Http.Expect msg
expectGraphQl toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
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
    Codec.prDataDecoder idFromVariables unresolvedCountDecoder


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
