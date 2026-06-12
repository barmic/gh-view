module Types exposing
    ( Author
    , Item
    , Mergeable(..)
    , PrData
    , PrId
    , PrState(..)
    , ReviewDecision(..)
    , isFinished
    , prUrl
    , sameId
    )

import Ci
import Time


{-| Stable identity of a watched pull request.
-}
type alias PrId =
    { owner : String
    , repo : String
    , number : Int
    }


{-| Outcome of GitHub's review decision (pullRequest.reviewDecision).
`NoReview` covers the `null` case (no review required yet).
-}
type ReviewDecision
    = Approved
    | ChangesRequested
    | ReviewRequired
    | NoReview


{-| GitHub computes mergeability asynchronously, hence `UnknownMergeable`.
-}
type Mergeable
    = Mergeable
    | Conflicting
    | UnknownMergeable


type PrState
    = StOpen
    | StClosed
    | StMerged


type alias Author =
    { login : String
    , avatarUrl : String
    }


{-| Everything we display for a single pull request.
-}
type alias PrData =
    { id : PrId
    , title : String
    , url : String
    , author : Author
    , headRef : String
    , baseRef : String
    , reviewDecision : ReviewDecision
    , unresolvedCount : Int
    , mergeable : Mergeable
    , state : PrState
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , ci : Ci.CiStatus
    }


{-| A watched PR in memory. `data` is `Nothing` until the first successful
fetch. `fetching` drives the per-row spinner.

Open PRs persist only their id (refetched on load); finished PRs persist a
frozen snapshot. That distinction is *derived* from the loaded state via
`isFinished`, so we never store it redundantly.
-}
type alias Item =
    { id : PrId
    , data : Maybe PrData
    , fetching : Bool
    }


{-| A finished PR (merged or closed) never changes again: we freeze its
snapshot and stop refetching it.
-}
isFinished : Item -> Bool
isFinished item =
    case item.data of
        Just d ->
            d.state == StClosed || d.state == StMerged

        Nothing ->
            False


sameId : PrId -> PrId -> Bool
sameId a b =
    a.owner == b.owner && a.repo == b.repo && a.number == b.number


prUrl : PrId -> String
prUrl id =
    "https://github.com/" ++ id.owner ++ "/" ++ id.repo ++ "/pull/" ++ String.fromInt id.number
