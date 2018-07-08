module UI.Button exposing (..)

import Element exposing (..)
import Element.Attributes exposing (center, alignLeft, fill, height, percent, px, verticalCenter, width, padding, spacing, center)
import Element.Events exposing (onClick)
import UI.Grid exposing (Size(..))
import UI.Style exposing (ButtonSemantics, ScoreButtonSemantics, Style)
import UI.Team
import Navigation
import Bets.Types exposing (HasQualified(..))
import Types exposing (Msg, Qualified(..), Topscorer)


pill : ButtonSemantics -> msg -> String -> Element Style variation msg
pill semantics msg buttonText =
    let
        buttonLayout =
            [ height (px 36), onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Centered [ padding 10 ] (text buttonText)
    in
        Element.column (UI.Style.Button semantics) buttonLayout [ textElement ]


fixedWidthPill : Float -> ButtonSemantics -> msg -> String -> Element Style variation msg
fixedWidthPill w semantics msg buttonText =
    let
        buttonLayout =
            [ width (px w), height (px 36), onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Centered [ padding 10 ] (text buttonText)
    in
        Element.column (UI.Style.Button semantics) buttonLayout [ textElement ]


navLink : ButtonSemantics -> msg -> String -> Element Style variation msg
navLink semantics msg buttonText =
    let
        buttonLayout =
            [ height (px 36), onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Centered [ padding 0 ] (text buttonText)
    in
        Element.column (UI.Style.NavLink semantics) buttonLayout [ textElement ]


submit : ButtonSemantics -> msg -> String -> Element Style variation msg
submit semantics msg buttonText =
    let
        buttonLayout =
            [ height (px 76), width (px 150), onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Centered [ padding 10 ] (text buttonText)
    in
        Element.column (UI.Style.Button semantics) buttonLayout [ textElement ]


button : Size -> ButtonSemantics -> msg -> String -> Element Style variation msg
button sz semantics msg buttonText =
    let
        ( w, h ) =
            case sz of
                XXL ->
                    ( width (percent 100), height (px 250) )

                XL ->
                    ( width (percent 40), height (px 76) )

                L ->
                    ( width (percent 30), height (px 76) )

                M ->
                    ( width (percent 24), height (px 76) )

                S ->
                    ( width (percent 16.66666), height (px 150) )

                XS ->
                    ( width (percent 10), height (px 76) )

                XXS ->
                    ( width (percent 10), height (px 40) )

                XXXS ->
                    ( width fill, height (px 40) )

        buttonLayout =
            [ w, h, onClick msg, Element.Attributes.center, Element.Attributes.verticalCenter ]
    in
        el (UI.Style.Button semantics) buttonLayout (text buttonText)


maybeTeamBadge :
    Qualified
    -> Maybe Bets.Types.Team
    -> Element Style variation msg
maybeTeamBadge semantics team =
    let
        w =
            width (px 64)

        h =
            height (px 76)

        buttonLayout =
            [ w, h, center, verticalCenter ]

        textElement =
            Element.el UI.Style.TeamName [] (UI.Team.viewMaybeTeamEl (team))
    in
        Element.column (UI.Style.TeamButton semantics) buttonLayout [ textElement ]


scoreButton : ScoreButtonSemantics -> msg -> String -> Element Style variation msg
scoreButton semantics msg buttonText =
    let
        w =
            px 48
                |> width

        h =
            px 28
                |> height

        buttonLayout =
            [ w, h, onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Score [] (text buttonText)
    in
        Element.column (UI.Style.ScoreButton semantics) buttonLayout [ textElement ]


topscorerBadge : HasQualified -> Topscorer -> msg -> Element Style variation msg
topscorerBadge hasQualified topscorer msg =
    let
        teamBadge =
            UI.Team.badge Types.NotYet topscorer.team

        semantics =
            case hasQualified of
                TBD ->
                    NotYet

                In ->
                    Did

                Out ->
                    DidNot
    in
        Element.row (UI.Style.TeamButton semantics)
            [ spacing 20, padding 10, verticalCenter, onClick msg ]
            [ teamBadge
            , Element.text topscorer.topscorer
            ]
