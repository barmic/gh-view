module View exposing (view)

import Ci
import Duration
import Html exposing (Attribute, Html, a, button, div, h1, header, img, input, span, text)
import Html.Attributes exposing (alt, class, classList, disabled, href, placeholder, src, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Model exposing (Model, Msg(..))
import Time
import Types exposing (Author, Item, Mergeable(..), PrData, PrId, PrState(..), ReviewDecision(..))


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewHeader model
        , viewError model.error
        , viewList model
        ]



-- HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "header" ]
        [ h1 [] [ text "gh-view" ]
        , viewTokenRow model
        , div [ class "add-row" ]
            [ input
                [ type_ "text"
                , placeholder "Coller l'URL d'une PR GitHub…"
                , value model.urlInput
                , onInput UrlChanged
                , onEnter AddClicked
                , class "input url-input"
                ]
                []
            , button [ onClick AddClicked, class "btn btn-primary" ] [ text "Ajouter" ]
            , button
                [ onClick RefreshClicked
                , class "btn"
                , disabled (anyFetching model)
                ]
                [ text (refreshLabel model) ]
            , span [ class "refresh-status" ] [ text (refreshStatus model) ]
            ]
        ]


{-| Hide the token input once a token is set: show a saved-status with a
"forget" button instead. Clearing the token brings the input back.
-}
viewTokenRow : Model -> Html Msg
viewTokenRow model =
    div [ class "token-row" ]
        (if String.isEmpty model.token then
            [ input
                [ type_ "password"
                , placeholder "Token GitHub"
                , value model.token
                , onInput TokenChanged
                , class "input token-input"
                ]
                []
            ]

         else
            [ span [ class "token-status" ] [ text "Token enregistré ✓" ]
            , button
                [ onClick ForgetToken, class "btn btn-ghost" ]
                [ text "Oublier" ]
            ]
        )


onEnter : Msg -> Attribute Msg
onEnter msg =
    Html.Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "ignored"
                )
        )


viewError : Maybe String -> Html Msg
viewError maybeError =
    case maybeError of
        Just message ->
            div [ class "banner banner-error" ] [ text message ]

        Nothing ->
            text ""



-- LIST


viewList : Model -> Html Msg
viewList model =
    if List.isEmpty model.items then
        div [ class "empty" ] [ text "Aucune PR suivie. Colle une URL ci-dessus pour commencer." ]

    else
        div [ class "list" ] (List.map (viewItem model) (sortItems model.items))


{-| OPEN PRs first (by updatedAt desc), MERGED/CLOSED last (also desc).
Not-yet-loaded items float to the top of the open group.
-}
sortItems : List Item -> List Item
sortItems =
    List.sortWith compareItems


compareItems : Item -> Item -> Order
compareItems a b =
    case ( Types.isFinished a, Types.isFinished b ) of
        ( False, True ) ->
            LT

        ( True, False ) ->
            GT

        _ ->
            compare (updatedKey b) (updatedKey a)


updatedKey : Item -> Int
updatedKey item =
    case item.data of
        Just d ->
            Time.posixToMillis d.updatedAt

        Nothing ->
            9007199254740991


viewItem : Model -> Item -> Html Msg
viewItem model item =
    let
        classes =
            classList
                [ ( "card", True )
                , ( "card-highlight", model.highlight == Just item.id )
                ]
    in
    case item.data of
        Nothing ->
            viewPending classes item

        Just d ->
            viewLoaded classes model.now item d


viewPending : Attribute Msg -> Item -> Html Msg
viewPending classes item =
    div [ classes ]
        [ div [ class "card-main" ]
            [ a [ href (Types.prUrl item.id), target "_blank", class "card-title" ]
                [ text (slug item.id) ]
            , span [ class "muted" ]
                [ text
                    (if item.fetching then
                        "Chargement…"

                     else
                        "Non chargée"
                    )
                ]
            ]
        , div [ class "card-actions" ] [ removeButton item.id ]
        ]


viewLoaded : Attribute Msg -> Time.Posix -> Item -> PrData -> Html Msg
viewLoaded classes now item d =
    div [ classes ]
        [ div [ class "card-main" ]
            [ div [ class "card-line" ]
                [ stateBadge d.state
                , a [ href d.url, target "_blank", class "card-title" ] [ text d.title ]
                ]
            , div [ class "card-sub" ]
                [ span [ class "repo" ] [ text (slug d.id) ]
                , viewBranch d
                , viewAuthor d.author
                ]
            ]
        , div [ class "card-badges" ]
            (reviewBadge d.reviewDecision
                :: commentsBadge d.unresolvedCount
                :: mergeableBadge d.mergeable
                :: ciBadges d
            )
        , div [ class "card-meta" ]
            [ span [ class "meta", title "Depuis la création" ] [ text ("créée " ++ Duration.relative now d.createdAt) ]
            , span [ class "meta", title "Depuis la dernière mise à jour" ] [ text ("maj " ++ Duration.relative now d.updatedAt) ]
            ]
        , div [ class "card-actions" ]
            [ if item.fetching then
                span [ class "spinner", title "Rafraîchissement…" ] []

              else
                text ""
            , removeButton d.id
            ]
        ]


viewBranch : PrData -> Html Msg
viewBranch d =
    span [ class "branch" ]
        (text d.headRef
            :: (if d.baseRef == "main" || d.baseRef == "master" then
                    []

                else
                    [ span [ class "branch-arrow" ] [ text " → " ], text d.baseRef ]
               )
        )


viewAuthor : Author -> Html Msg
viewAuthor author =
    span [ class "author" ]
        [ if String.isEmpty author.avatarUrl then
            text ""

          else
            img [ src author.avatarUrl, class "avatar", alt "" ] []
        , text ("@" ++ author.login)
        ]


removeButton : PrId -> Html Msg
removeButton id =
    button
        [ onClick (RemoveClicked id)
        , class "btn-remove"
        , title "Retirer de la watch list"
        ]
        [ text "✕" ]



-- BADGES


stateBadge : PrState -> Html Msg
stateBadge state =
    case state of
        StOpen ->
            span [ class "badge badge-open" ] [ text "Open" ]

        StMerged ->
            span [ class "badge badge-merged" ] [ text "Merged" ]

        StClosed ->
            span [ class "badge badge-closed" ] [ text "Closed" ]


reviewBadge : ReviewDecision -> Html Msg
reviewBadge rd =
    case rd of
        Approved ->
            span [ class "badge badge-ok" ] [ text "✓ Approuvée" ]

        ChangesRequested ->
            span [ class "badge badge-warn" ] [ text "✗ Changements demandés" ]

        ReviewRequired ->
            span [ class "badge badge-neutral" ] [ text "En attente de review" ]

        NoReview ->
            span [ class "badge badge-neutral" ] [ text "Sans review" ]


commentsBadge : Int -> Html Msg
commentsBadge n =
    if n == 0 then
        span [ class "badge badge-ok" ] [ text "0 commentaire" ]

    else
        span [ class "badge badge-warn" ] [ text (String.fromInt n ++ " à traiter") ]


mergeableBadge : Mergeable -> Html Msg
mergeableBadge m =
    case m of
        Mergeable ->
            span [ class "badge badge-ok" ] [ text "Mergeable" ]

        Conflicting ->
            span [ class "badge badge-warn" ] [ text "Conflits" ]

        UnknownMergeable ->
            span [ class "badge badge-neutral" ] [ text "Mergeable ?" ]


{-| CI badges (GitHub Actions, then CircleCI). Only for open PRs — a finished
PR's CI is irrelevant and not persisted. A provider with no checks (`Unknown`)
renders nothing.
-}
ciBadges : PrData -> List (Html Msg)
ciBadges d =
    if d.state == StOpen then
        [ ciBadge "GHA" d.ci.gha, ciBadge "CircleCI" d.ci.circle ]

    else
        []


ciBadge : String -> Ci.ProviderCi -> Html Msg
ciBadge label provider =
    case provider.state of
        Ci.Unknown ->
            text ""

        _ ->
            let
                content =
                    [ text (ciLabel label provider.state) ]
            in
            case provider.url of
                Just url ->
                    a
                        [ href url
                        , target "_blank"
                        , class ("badge badge-link " ++ ciClass provider.state)
                        ]
                        content

                Nothing ->
                    span [ class ("badge " ++ ciClass provider.state) ] content


ciClass : Ci.CiState -> String
ciClass state =
    case state of
        Ci.Succeed ->
            "badge-ok"

        Ci.Failed ->
            "badge-warn"

        _ ->
            "badge-neutral"


ciLabel : String -> Ci.CiState -> String
ciLabel label state =
    case state of
        Ci.Succeed ->
            "✓ " ++ label

        Ci.Failed ->
            "✗ " ++ label

        _ ->
            label ++ "…"



-- HELPERS


slug : PrId -> String
slug id =
    id.owner ++ "/" ++ id.repo ++ " #" ++ String.fromInt id.number


anyFetching : Model -> Bool
anyFetching model =
    List.any .fetching model.items


refreshLabel : Model -> String
refreshLabel model =
    if anyFetching model then
        "Rafraîchissement…"

    else
        "Rafraîchir les PR ouvertes"


refreshStatus : Model -> String
refreshStatus model =
    case model.lastRefresh of
        Just t ->
            "rafraîchi il y a " ++ Duration.relative model.now t

        Nothing ->
            "jamais"
