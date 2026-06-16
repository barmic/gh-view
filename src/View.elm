module View exposing (view, viewBadges, viewConfig)

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
        , viewNotice model.notice
        , viewList model
        ]



-- HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "header" ]
        [ h1 [] [ text "gh-view" ]
        , viewTokenRow model
        , viewConfig model
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
            , button
                [ onClick FetchNewClicked
                , class "btn"
                , disabled model.discovering
                , title "Découvrir les PR ouvertes des dépôts/comptes configurés et celles qui te sont assignées"
                ]
                [ text (discoverLabel model) ]
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


{-| Collapsible discovery configuration: the global lists of watched repos
(`owner/repo`) and authors (logins). Each list has a text input + "Ajouter"
and renders its entries as removable chips.
-}
viewConfig : Model -> Html Msg
viewConfig model =
    div [ class "config" ]
        (button
            [ onClick ToggleConfig, class "btn btn-ghost config-toggle" ]
            [ text
                ((if model.configOpen then
                    "▾ "

                  else
                    "▸ "
                 )
                    ++ "Configuration de la découverte"
                )
            ]
            :: (if model.configOpen then
                    [ div [ class "config-body" ]
                        [ viewConfigGroup "Dépôts" "owner/repo" model.repoInput RepoInputChanged AddRepoClicked model.repos RemoveRepoClicked
                        , viewConfigGroup "Comptes" "login GitHub (avec ou sans @)" model.authorInput AuthorInputChanged AddAuthorClicked model.authors RemoveAuthorClicked
                        ]
                    ]

                else
                    []
               )
        )


viewConfigGroup : String -> String -> String -> (String -> Msg) -> Msg -> List String -> (String -> Msg) -> Html Msg
viewConfigGroup label ph inputValue onInputMsg onAddMsg items onRemoveMsg =
    div [ class "config-group" ]
        [ span [ class "config-label" ] [ text label ]
        , div [ class "config-row" ]
            [ input
                [ type_ "text"
                , placeholder ph
                , value inputValue
                , onInput onInputMsg
                , onEnter onAddMsg
                , class "input config-input"
                ]
                []
            , button [ onClick onAddMsg, class "btn btn-primary" ] [ text "Ajouter" ]
            ]
        , div [ class "chips" ] (List.map (viewChip onRemoveMsg) items)
        ]


viewChip : (String -> Msg) -> String -> Html Msg
viewChip onRemoveMsg item =
    span [ class "chip" ]
        [ text item
        , button
            [ onClick (onRemoveMsg item), class "chip-remove", title "Retirer" ]
            [ text "✕" ]
        ]


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


viewNotice : Maybe String -> Html Msg
viewNotice maybeNotice =
    case maybeNotice of
        Just message ->
            div [ class "banner banner-info" ] [ text message ]

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
        , div [ class "card-right" ]
            [ div [ class "card-actions" ] [ removeButton item.id ] ]
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
        , viewBadges d
        , div [ class "card-right" ]
            [ div [ class "card-meta" ]
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


{-| The card's badge cluster: a first row with the review/comments/mergeable
status badges, and a second CI row (GitHub Actions, CircleCI). The CI row is
omitted entirely when no provider reports a state, so the card stays compact.
-}
viewBadges : PrData -> Html Msg
viewBadges d =
    div [ class "card-badges" ]
        (div [ class "badge-row" ]
            [ reviewBadge d.reviewDecision
            , commentsBadge d.unresolvedCount
            , mergeableBadge d.mergeable
            ]
            :: (case ciBadges d of
                    [] ->
                        []

                    badges ->
                        [ div [ class "badge-row badge-row-ci" ] badges ]
               )
        )


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
contributes nothing, so an open PR without any CI yields an empty list and the
CI row collapses.
-}
ciBadges : PrData -> List (Html Msg)
ciBadges d =
    if d.state == StOpen then
        List.filterMap identity
            [ ciBadge "GHA" d.ci.gha, ciBadge "CircleCI" d.ci.circle ]

    else
        []


ciBadge : String -> Ci.ProviderCi -> Maybe (Html Msg)
ciBadge label provider =
    case provider.state of
        Ci.Unknown ->
            Nothing

        _ ->
            let
                content =
                    [ text (ciLabel label provider.state) ]
            in
            Just
                (case provider.url of
                    Just url ->
                        a
                            [ href url
                            , target "_blank"
                            , class ("badge badge-link " ++ ciClass provider.state)
                            ]
                            content

                    Nothing ->
                        span [ class ("badge " ++ ciClass provider.state) ] content
                )


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


discoverLabel : Model -> String
discoverLabel model =
    if model.discovering then
        "Récupération…"

    else
        "Récupérer les nouvelles PR"


refreshStatus : Model -> String
refreshStatus model =
    case model.lastRefresh of
        Just t ->
            "rafraîchi il y a " ++ Duration.relative model.now t

        Nothing ->
            "jamais"
