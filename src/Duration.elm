module Duration exposing (relative)

{-| Compact relative duration between `t` and `now` (e.g. "3j", "5h", "30min").
Picks the largest meaningful unit.
-}

import Time


relative : Time.Posix -> Time.Posix -> String
relative now t =
    let
        secs =
            (Time.posixToMillis now - Time.posixToMillis t) // 1000
    in
    if secs < 0 then
        "à venir"

    else if secs < 60 then
        "<1min"

    else if secs < 3600 then
        String.fromInt (secs // 60) ++ "min"

    else if secs < 86400 then
        String.fromInt (secs // 3600) ++ "h"

    else if secs < 604800 then
        String.fromInt (secs // 86400) ++ "j"

    else if secs < 2592000 then
        String.fromInt (secs // 604800) ++ "sem"

    else if secs < 31536000 then
        String.fromInt (secs // 2592000) ++ "mois"

    else
        String.fromInt (secs // 31536000) ++ "an"
