module Favicon exposing (dataUri, title)

{-| Dynamic eye-shaped favicon and tab title reflecting the number of watched
open PRs.

  - Open eye with the count in the pupil when there is at least one open PR.
  - Closed (sleepy) eye when there is nothing to watch.
  - Favicon caps at "9+"; the title carries the exact count.

-}

import Url


primary : String
primary =
    "#5b3f9e"


pupilText : String
pupilText =
    "#ffffff"


{-| `data:` URI for an SVG favicon. The whole SVG is percent-encoded, which is
the most robust way to inline arbitrary markup in a data URI.
-}
dataUri : Int -> String
dataUri count =
    "data:image/svg+xml," ++ Url.percentEncode (svg count)


{-| Exact count (not capped) as a tab-title prefix.
-}
title : Int -> String
title count =
    if count <= 0 then
        "gh-view"

    else
        "(" ++ String.fromInt count ++ ") gh-view"


svg : Int -> String
svg count =
    wrap
        (if count <= 0 then
            closedEye

         else
            openEye (label count)
        )


wrap : String -> String
wrap inner =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'>" ++ inner ++ "</svg>"


label : Int -> String
label count =
    if count > 9 then
        "9+"

    else
        String.fromInt count


openEye : String -> String
openEye text =
    let
        -- A single glyph can be larger than the two-char "9+".
        fontSize =
            if String.length text > 1 then
                "17"

            else
                "22"
    in
    String.join ""
        [ "<ellipse cx='32' cy='32' rx='30' ry='20' fill='#ffffff' stroke='" ++ primary ++ "' stroke-width='4'/>"
        , "<circle cx='32' cy='32' r='15' fill='" ++ primary ++ "'/>"
        , "<text x='32' y='33' font-family='Arial,Helvetica,sans-serif' font-size='" ++ fontSize ++ "' font-weight='700' fill='" ++ pupilText ++ "' text-anchor='middle' dominant-baseline='central'>" ++ text ++ "</text>"
        ]


closedEye : String
closedEye =
    String.join ""
        [ "<path d='M8,30 Q32,48 56,30' fill='none' stroke='" ++ primary ++ "' stroke-width='5' stroke-linecap='round'/>"
        , "<path d='M14,40 l-4,5' stroke='" ++ primary ++ "' stroke-width='3' stroke-linecap='round'/>"
        , "<path d='M32,44 l0,6' stroke='" ++ primary ++ "' stroke-width='3' stroke-linecap='round'/>"
        , "<path d='M50,40 l4,5' stroke='" ++ primary ++ "' stroke-width='3' stroke-linecap='round'/>"
        ]
