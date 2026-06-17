module Discover exposing (assignedQuery, authorQuery, merge, windowDays)

{-| Pure logic for PR discovery: building the GitHub search query strings and
merging discovered PRs into the existing watch list. The HTTP/JSON plumbing
lives in `GitHub`; everything here is unit-testable without a network.
-}

import Iso8601
import Time
import Types exposing (Item, PrData)


{-| How far back the author/repo discovery search looks, based on each PR's
last activity (`updated`). PRs assigned to the viewer are never time-limited.
-}
windowDays : Int
windowDays =
    31


{-| The search that always runs: open PRs assigned to the authenticated user,
anywhere. `@me` is resolved server-side, so no login is needed. Drafts are
excluded (`-is:draft`): they are generally not actionable.
-}
assignedQuery : String
assignedQuery =
    "is:open is:pr -is:draft assignee:@me"


{-| The author search: open PRs in the configured repos whose author is one of
the configured logins, restricted to those active in the last `windowDays`
(`updated:>=`). Drafts are excluded (`-is:draft`). Returns `Nothing` (skip the
search) unless both lists are non-empty, since "PRs of my projects by configured
authors" needs both.
-}
authorQuery : Time.Posix -> List String -> List String -> Maybe String
authorQuery now repos authors =
    if List.isEmpty repos || List.isEmpty authors then
        Nothing

    else
        Just
            (String.join " "
                ("is:open is:pr -is:draft"
                    :: List.map (\r -> "repo:" ++ r) repos
                    ++ List.map (\a -> "author:" ++ a) authors
                    ++ [ "updated:>=" ++ Iso8601.fromTime (windowStart now) ]
                )
            )


{-| The earliest activity date a discovered author PR may have: `now` minus the
discovery window.
-}
windowStart : Time.Posix -> Time.Posix
windowStart now =
    Time.millisToPosix (Time.posixToMillis now - windowDays * 24 * 60 * 60 * 1000)


{-| Fold discovered PRs into the current items: refresh the data of items
already present (matched by id), append the genuinely new ones, and drop
duplicates among the discovered PRs themselves. Existing order is preserved.
-}
merge : List PrData -> List Item -> List Item
merge prs items =
    let
        refreshed =
            List.map
                (\it ->
                    case find (\d -> Types.sameId d.id it.id) prs of
                        Just d ->
                            { it | data = Just d, fetching = False }

                        Nothing ->
                            it
                )
                items

        isNew d =
            not (List.any (\it -> Types.sameId it.id d.id) items)

        newItems =
            prs
                |> List.filter isNew
                |> dedupById
                |> List.map (\d -> { id = d.id, data = Just d, fetching = False })
    in
    refreshed ++ newItems


find : (a -> Bool) -> List a -> Maybe a
find pred =
    List.filter pred >> List.head


dedupById : List PrData -> List PrData
dedupById prs =
    List.foldl
        (\d acc ->
            if List.any (\seen -> Types.sameId seen.id d.id) acc then
                acc

            else
                acc ++ [ d ]
        )
        []
        prs
