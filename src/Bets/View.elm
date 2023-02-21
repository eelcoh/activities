module Bets.View exposing (..)

import Bets.Bet
import Bets.Types exposing (..)
import Bets.Types.Bracket as B
import Bets.Types.Match as M
import Bets.Types.Score as S
import Element exposing (alignLeft, alignRight, center, column, height, padding, paddingBottom, paddingLeft, paddingTop, paddingXY, row, spacing, spread, verticalCenter, verticalSpread, width, x)
import Html exposing (Html, button, div, input, section, text, textarea)
import Types exposing (Msg, Qualified(..), ScreenSize)
import UI.Button
import UI.Size exposing (bodyWidth)
import UI.Style
import UI.Team exposing (viewTeamAway, viewTeamHome)
import UI.Text


viewBet : Bet -> ScreenSize -> Element.Element UI.Style.Style variation Msg
viewBet bet screenSize =
    let
        w =
            bodyWidth screenSize
    in
    Element.column UI.Style.None
        [ spacing 20, w ]
        [ displayParticipant bet

        --, intro
        , UI.Text.header2 "Het Schema"
        , displayBracket bet
        , UI.Text.header2 "De Topscorer"
        , topscorerIntro
        , displayTopscorer bet
        , UI.Text.header2 "De wedstrijden"
        , matchesIntro
        , displayMatches bet.answers
        ]



-- type AnswerT
--     = AnswerGroupMatch Group Match (Maybe Score) Points
--     | AnswerGroupPosition Group Pos Draw Points
--     | AnswerGroupBestThirds BestThirds Points
--     | AnswerMatchWinner Round Match NextID (Maybe Team) Points
--     | AnswerBracket Bracket Points
--     | AnswerTopscorer Topscorer Points
--     | AnswerParticipant Participant


intro : Element.Element UI.Style.Style variation Msg
intro =
    let
        introtext =
            """Dank voor je inzending!
            """
    in
    Element.paragraph UI.Style.None
        []
        [ UI.Text.simpleText introtext
        ]


matchesIntro : Element.Element UI.Style.Style variation Msg
matchesIntro =
    let
        introtext =
            """Voor iedere wedstrijd die je helemaal goed hebt voorspeld krijg je 3 punten.
            Heb je enkel de toto goed, krijg je 1 punt. En anders niets.
            """
    in
    Element.paragraph UI.Style.None [] [ UI.Text.simpleText introtext ]


topscorerIntro : Element.Element UI.Style.Style variation Msg
topscorerIntro =
    let
        introtext =
            """De topscorer levert 9 punten op, mits goed voorspeld natuurlijk.
            """
    in
    Element.paragraph UI.Style.None [] [ UI.Text.simpleText introtext ]


displayMatches :
    List ( AnswerID, AnswerT )
    -> Element.Element UI.Style.Style variation Msg
displayMatches answers =
    Element.wrappedRow UI.Style.Matches
        [ padding 10, spacing 7, center ]
        (List.filterMap displayMatch answers)


displayMatch : ( AnswerID, AnswerT ) -> Maybe (Element.Element UI.Style.Style variation Msg)
displayMatch ( answerId, answer ) =
    let
        disp match mScore pts =
            let
                home =
                    UI.Team.viewMaybeTeamEl (M.homeTeam match)

                away =
                    UI.Team.viewMaybeTeamEl (M.awayTeam match)

                sc =
                    displayScore mScore
            in
            Element.row (UI.Style.MatchRow pts)
                [ spread, verticalCenter, paddingXY 10 5, spacing 7, width (px 150), height (px 70) ]
                [ home, sc, away ]
    in
    case answer of
        AnswerGroupMatch g match mScore pts ->
            Just <| disp match mScore pts

        _ ->
            Nothing


scoreString : a -> b -> String
scoreString h a =
    List.foldr (++) "" [ " ", toString h, "-", toString a, " " ]


displayScore : Maybe Score -> Element.Element UI.Style.Style variation msg
displayScore mScore =
    let
        txt =
            case mScore of
                Just score ->
                    S.asString score

                Nothing ->
                    " _-_ "
    in
    Element.el UI.Style.Score [ verticalCenter ] (Element.text txt)



-- --
-- module Form.Questions.Bracket exposing (Msg, update, view)
-- import Bets.Bet exposing (setTeam)
-- import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Bracket(..), Qualifier, Slot, Team, Winner(..))
-- import UI.Text
-- import Bets.Types.Bracket as B
-- import Element
-- import Element.Attributes exposing (alignRight, center, px, spacing, paddingXY, spread, width)
-- import Form.Questions.Types exposing (QState)
-- import Html exposing (..)
-- import UI.Button
-- import UI.Style
-- type Msg
--     = SetWinner AnswerID Slot Winner


type IsWinner
    = Yes
    | No
    | Undecided


isWinner : Winner -> Winner -> IsWinner
isWinner bracketWinner homeOrAway =
    case bracketWinner of
        None ->
            Undecided

        _ ->
            if homeOrAway == bracketWinner then
                Yes

            else
                No


displayBracket : Bet -> Element.Element UI.Style.Style variation Msg
displayBracket bet =
    let
        mAnswer =
            Bets.Bet.getAnswer bet "br"

        introtext =
            """Dit is het schema voor de tweede ronde en verder. In het midden staat de finale en de kampioen,
         daarboven en onder de ronden die daaraan voorafgaan. Voor ieder team dat je juist hebt in de tweede
         ronde krijg je 1 punt. Voor de juiste kwartfinalisten krijg je 4 punten. Halve finalisten leveren 7
         punten op, finalisten 10 punten en de kampioen 13 punten."""

        introduction =
            Element.paragraph UI.Style.None [] [ UI.Text.simpleText introtext ]
    in
    case mAnswer of
        Just (( answerId, AnswerBracket bracket _ ) as answer) ->
            Element.column UI.Style.None
                [ spacing 20 ]
                [ introduction
                , viewBracket bet answer bracket
                ]

        _ ->
            Element.empty


viewBracket : Bet -> Answer -> Bracket -> Element.Element UI.Style.Style variation Msg
viewBracket bet answer bracket =
    {-
       mn37 = MatchNode "m37" None tnra tnrc -- "2016/06/15 15:00" saintetienne (Just "W37")
       mn38 = MatchNode "m38" None tnwb tnt2 -- "2016/06/15 15:00" paris (Just "W38")
       mn39 = MatchNode "m39" None tnwd tnt4 -- "2016/06/15 15:00" lens (Just "W39")
       mn40 = MatchNode "m40" None tnwa tnt1 -- "2016/06/15 15:00" lyon (Just "W40")
       mn41 = MatchNode "m41" None tnwc tnt3 -- "2016/06/15 15:00" lille (Just "W41")
       mn42 = MatchNode "m42" None tnwf tnre -- "2016/06/15 15:00" toulouse (Just "W42")
       mn43 = MatchNode "m43" None tnwe tnrd -- "2016/06/15 15:00" saintdenis (Just "W43")
       mn44 = MatchNode "m44" None tnrb tnrf -- "2016/06/15 15:00" nice (Just "W44")

       mn45 = MatchNode "m45" None mn37 mn39 -- "2016/06/15 15:00" marseille (Just "W45")
       mn46 = MatchNode "m46" None mn38 mn42 --  "2016/06/15 15:00" lille (Just "W46")
       mn47 = MatchNode "m47" None mn41 mn43 -- "2016/06/15 15:00" bordeaux (Just "W47")
       mn48 = MatchNode "m48" None mn40 mn44 -- "2016/06/15 15:00" saintdenis (Just "W48")

       mn49 = MatchNode "m49" None mn45 mn46 -- "2016/06/15 15:00" lyon (Just "W49")
       mn50 = MatchNode "m50" None mn47 mn48 -- "2016/06/15 15:00" marseille (Just "W50")

       mn51 = MatchNode "m51" None mn49 mn50 -- "2016/06/15 15:00" saintdenis Nothing
    -}
    let
        v mb =
            viewMatchWinner bet answer mb

        final =
            B.get bracket "m64"

        m49 =
            v <| B.get bracket "m49"

        m50 =
            v <| B.get bracket "m50"

        m51 =
            v <| B.get bracket "m51"

        m52 =
            v <| B.get bracket "m52"

        m53 =
            v <| B.get bracket "m53"

        m54 =
            v <| B.get bracket "m54"

        m55 =
            v <| B.get bracket "m55"

        m56 =
            v <| B.get bracket "m56"

        m57 =
            v <| B.get bracket "m57"

        m58 =
            v <| B.get bracket "m58"

        m59 =
            v <| B.get bracket "m59"

        m60 =
            v <| B.get bracket "m60"

        m61 =
            v <| B.get bracket "m61"

        m62 =
            v <| B.get bracket "m62"

        m64 =
            v <| B.get bracket "m64"

        champion =
            mkButtonChamp final
    in
    Element.column UI.Style.None
        [ spacing 10, width (px 600) ]
        [ Element.row UI.Style.None [ spread ] [ m49, m50, m53, m54 ]
        , Element.row UI.Style.None [ spread, paddingXY 76 0 ] [ m57, m58 ]
        , Element.row UI.Style.None [ center ] [ m61 ]
        , Element.row UI.Style.None [ alignRight, spacing 44 ] [ m64, champion ]
        , Element.row UI.Style.None [ center ] [ m62 ]
        , Element.row UI.Style.None [ spread, paddingXY 76 0 ] [ m59, m60 ]
        , Element.row UI.Style.None [ spread ] [ m51, m52, m55, m56 ]
        ]


viewMatchWinner :
    a
    -> ( AnswerID, a2 )
    -> Maybe Bracket
    -> Element.Element UI.Style.Style variation Msg
viewMatchWinner bet answer mBracket =
    case mBracket of
        Just (MatchNode slot winner home away rd hasQ) ->
            let
                homeHasQ =
                    didQualify home

                awayHasQ =
                    didQualify away

                homeButton =
                    mkButton answer HomeTeam slot homeHasQ home

                awayButton =
                    mkButton answer AwayTeam slot awayHasQ away

                dash =
                    text " - "
            in
            Element.row UI.Style.None [ spacing 7 ] [ homeButton, awayButton ]

        _ ->
            Element.empty


didQualify : Bracket -> HasQualified
didQualify b =
    case b of
        MatchNode _ _ _ _ _ hasQ ->
            hasQ

        TeamNode _ _ hasQ ->
            hasQ


mkButton :
    ( AnswerID, a2 )
    -> Winner
    -> Slot
    -> HasQualified
    -> Bracket
    -> Element.Element UI.Style.Style variation Msg
mkButton answer wnnr slot hasQualified bracket =
    let
        s =
            case hasQualified of
                In ->
                    Did

                Out ->
                    DidNot

                TBD ->
                    NotYet

        answerId =
            Tuple.first answer

        attrs =
            []

        team =
            B.qualifier bracket
    in
    UI.Button.maybeTeamBadge s team


mkButtonChamp : Maybe Bracket -> Element.Element UI.Style.Style variation msg
mkButtonChamp mBracket =
    let
        mTeam =
            mBracket
                |> Maybe.andThen B.winner

        qualified =
            mBracket
                |> Maybe.map didQualify
                |> Maybe.map toQualified
                |> Maybe.withDefault NotYet

        toQualified h =
            case h of
                In ->
                    Did

                Out ->
                    DidNot

                TBD ->
                    NotYet

        attrs =
            []
    in
    UI.Button.maybeTeamBadge qualified mTeam


displayTopscorer : Bet -> Element.Element UI.Style.Style variation Msg
displayTopscorer bet =
    let
        mkTopscorer ts points =
            let
                hq =
                    case points of
                        Just 0 ->
                            Out

                        Just 9 ->
                            In

                        _ ->
                            TBD

                t =
                    case ts of
                        ( Just topscorer, Just team ) ->
                            Types.Topscorer team topscorer
                                |> Just

                        _ ->
                            Nothing
            in
            Maybe.map (\tops -> UI.Button.topscorerBadge hq tops Types.None) t

        makeTopscorerBadge answer =
            case answer of
                ( _, AnswerTopscorer topscorer points ) ->
                    mkTopscorer topscorer points

                _ ->
                    Nothing

        mTopscorerBadge =
            Bets.Bet.getAnswer bet "ts"
                |> Maybe.andThen makeTopscorerBadge
    in
    Maybe.withDefault Element.empty mTopscorerBadge


error : String -> Element.Element UI.Style.Style variation msg
error text =
    Element.row UI.Style.Error [] [ Element.text text ]


displayParticipant : Bet -> Element.Element UI.Style.Style variation msg
displayParticipant bet =
    let
        mAnswer =
            Bets.Bet.getAnswer bet "me"

        h part =
            part.name
                |> Maybe.map UI.Text.header1
                |> Maybe.withDefault (errorBox "whoeps")

        residenceText =
            (++) "uit "

        p part =
            Maybe.map residenceText part.residence
                |> Maybe.map UI.Text.simpleText
                |> Maybe.withDefault (UI.Text.simpleText "onbekend")
    in
    case mAnswer of
        Just (( answerId, AnswerParticipant part ) as answer) ->
            Element.column UI.Style.None
                [ spacing 20, verticalCenter ]
                [ h part
                , p part
                ]

        _ ->
            Element.empty


errorBox : String -> Element.Element UI.Style.Style variation msg
errorBox text =
    Element.row UI.Style.Error [] [ Element.text text ]
