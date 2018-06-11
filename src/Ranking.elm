module Ranking exposing (viewRanking, recreate, fetchRanking)

import Types exposing (Model, Activity(..), Msg(..), RankingSummary, RankingSummaryLine, RoundScore, Token(..), RankingGroup)
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web
import Http
import Json.Encode
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Element exposing (column, row)
import Element.Attributes exposing (spread, px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Style
import UI.Button
import UI.Text
import Date


fetchRanking : Cmd Msg
fetchRanking =
    Web.get "/bets/ranking/" FetchedRanking decode


recreate : Token -> Cmd Msg
recreate (Token token) =
    -- Web.post "/bets/ranking/initial/" FetchedRanking decode Json.Encode.null
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { headers = [ header ]
            , withCredentials = True
            , timeout = Nothing
            }
    in
        Web.postWithConfig config "/bets/ranking/initial/" FetchedRanking decode Json.Encode.null


viewRanking : Model -> Element.Element UI.Style.Style variation Msg
viewRanking model =
    let
        items =
            case model.token of
                Success _ ->
                    [ adminBox model
                    , viewRankingGroups model
                    ]

                _ ->
                    [ viewRankingGroups model ]
    in
        Element.column UI.Style.None
            []
            items


adminBox : Model -> Element.Element UI.Style.Style variation Msg
adminBox model =
    UI.Button.pill UI.Style.Active RecreateRanking "recreate"


viewRankingGroups : Model -> Element.Element UI.Style.Style variation msg
viewRankingGroups model =
    case model.ranking of
        Success ranking ->
            let
                header =
                    viewRankingHeader

                rank =
                    (List.map viewRankingGroup ranking.summary)

                datetxt =
                    Element.el UI.Style.AuthorText
                        [ alignRight, paddingXY 0 10 ]
                        (Element.text ("bijgewerkt op " ++ (UI.Text.dateText ranking.time)))

                column =
                    (header :: rank) ++ [ datetxt ]
            in
                Element.column UI.Style.None
                    [ paddingBottom 50 ]
                    column

        NotAsked ->
            Element.text "nog niet opgevraagd"

        Loading ->
            Element.text "aan het ophalen..."

        Failure _ ->
            UI.Text.error "oei, daar ging iets niet helemaal goed"


viewRankingHeader : Element.Element UI.Style.Style variation msg
viewRankingHeader =
    Element.row UI.Style.RankingHeader
        [ paddingXY 0 5, spread ]
        [ Element.el UI.Style.RankingPosH [ width (px 20) ] (Element.text "#")
        , Element.el UI.Style.RankingNameH [ width (px 200) ] (Element.text "Naam")
        , Element.el UI.Style.RankingPointsH [ width (px 80) ] (Element.text "Punten")
        ]


viewRankingGroup : RankingGroup -> Element.Element UI.Style.Style variation msg
viewRankingGroup grp =
    Element.row UI.Style.RankingGroup
        [ paddingXY 0 5, spread ]
        [ Element.el UI.Style.RankingPos [ width (px 20) ] (Element.text (Basics.toString grp.pos))
        , viewRankingLines grp.bets
        , Element.el UI.Style.RankingPoints [ width (px 80) ] (Element.text (Basics.toString grp.total))
        ]


viewRankingLines : List RankingSummaryLine -> Element.Element UI.Style.Style variation msg
viewRankingLines lines =
    Element.column UI.Style.None
        [ width (px 200), paddingBottom 4 ]
        (List.map viewRankingLine lines)


viewRankingLine : RankingSummaryLine -> Element.Element UI.Style.Style variation msg
viewRankingLine line =
    Element.el UI.Style.RankingName [ width (px 200) ] (Element.text line.name)


decode : Decoder RankingSummary
decode =
    Json.Decode.map2 RankingSummary
        (field "summary" (Json.Decode.list decodeRankingRankingGroup))
        (field "time" decodeDate)


decodeRankingRankingGroup : Decoder RankingGroup
decodeRankingRankingGroup =
    Json.Decode.map3 RankingGroup
        (field "pos" Json.Decode.int)
        (field "bets" (Json.Decode.list decodeRankingSummaryLine))
        (field "total" Json.Decode.int)


decodeRankingSummaryLine : Decoder RankingSummaryLine
decodeRankingSummaryLine =
    Json.Decode.map5 RankingSummaryLine
        (field "name" Json.Decode.string)
        (field "rounds" (Json.Decode.list decodeRoundScore))
        (field "topscorer" Json.Decode.int)
        (field "total" Json.Decode.int)
        (field "uuid" Json.Decode.string)


decodeRoundScore : Decoder RoundScore
decodeRoundScore =
    Json.Decode.map2 RoundScore
        (field "round" Json.Decode.string)
        (field "points" Json.Decode.int)


decodeDate : Decoder Date.Date
decodeDate =
    Json.Decode.float
        |> Json.Decode.map Date.fromTime
