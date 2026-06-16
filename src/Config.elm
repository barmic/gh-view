module Config exposing (addItem, normalizeAuthor, parseRepo, removeItem)

{-| Pure logic for the discovery configuration: the global lists of watched
repositories (`owner/repo` slugs) and authors (GitHub logins). Validation and
normalisation live here so they can be unit-tested without the UI.
-}


{-| Drop surrounding whitespace and a single leading `@` so `@octocat` and
`octocat` normalise to the same stored login.
-}
normalizeAuthor : String -> String
normalizeAuthor raw =
    let
        trimmed =
            String.trim raw
    in
    if String.startsWith "@" trimmed then
        String.dropLeft 1 trimmed

    else
        trimmed


{-| Validate an `owner/repo` slug, returning its trimmed canonical form. Both
segments must be present and non-empty; anything else is rejected.
-}
parseRepo : String -> Maybe String
parseRepo raw =
    case String.split "/" (String.trim raw) of
        [ owner, repo ] ->
            if String.isEmpty owner || String.isEmpty repo then
                Nothing

            else
                Just (owner ++ "/" ++ repo)

        _ ->
            Nothing


{-| Append `item` unless an equal entry (case-insensitive) is already present.
-}
addItem : String -> List String -> List String
addItem item items =
    if List.any (sameKey item) items then
        items

    else
        items ++ [ item ]


{-| Remove every case-insensitive match of `item`.
-}
removeItem : String -> List String -> List String
removeItem item items =
    List.filter (not << sameKey item) items


sameKey : String -> String -> Bool
sameKey a b =
    String.toLower a == String.toLower b
