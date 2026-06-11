module PrUrl exposing (parse)

{-| Parse a GitHub PR URL into a `PrId`, tolerant of scheme, trailing slash,
sub-paths (`/files`), query strings and fragments.
-}

import Types exposing (PrId)


parse : String -> Maybe PrId
parse raw =
    raw
        |> String.trim
        |> String.split "/"
        |> List.filter (not << String.isEmpty)
        |> findPull


findPull : List String -> Maybe PrId
findPull parts =
    case parts of
        owner :: repo :: "pull" :: numRaw :: _ ->
            numRaw
                |> String.toList
                |> takeWhile Char.isDigit
                |> String.fromList
                |> String.toInt
                |> Maybe.map (PrId owner repo)

        _ :: rest ->
            findPull rest

        [] ->
            Nothing


takeWhile : (a -> Bool) -> List a -> List a
takeWhile pred xs =
    case xs of
        x :: rest ->
            if pred x then
                x :: takeWhile pred rest

            else
                []

        [] ->
            []
