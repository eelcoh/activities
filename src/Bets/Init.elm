module Bets.Init exposing (bet)

import Bets.Types exposing (..)
import Bets.Types.Team exposing (team)
import Bets.Types.Participant as P
import Bets.Types.Date exposing (toDate)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


bet : Bet
bet =
    { answers = answers
    , betId = Nothing
    , uuid = Nothing
    , active = True
    }


answers : Answers
answers =
    let
        a1 =
            "A1" => Just (team "RUS" "Rusland")

        a2 =
            "A2" => Just (team "KSA" "Saoedi Arabië")

        a3 =
            "A3" => Just (team "EGY" "Egypte")

        a4 =
            "A4" => Just (team "URU" "Uruguay")

        b1 =
            "B1" => Just (team "POR" "Portugal")

        b2 =
            "B2" => Just (team "ESP" "Spanje")

        b3 =
            "B3" => Just (team "MAR" "Marokko")

        b4 =
            "B4" => Just (team "IRN" "Iran")

        c1 =
            "C1" => Just (team "FRA" "Frankrijk")

        c2 =
            "C2" => Just (team "AUS" "Australië")

        c3 =
            "C3" => Just (team "PER" "Peru")

        c4 =
            "C4" => Just (team "DEN" "Denmark")

        d1 =
            "D1" => Just (team "ARG" "Argentinë")

        d2 =
            "D2" => Just (team "ISL" "IJsland")

        d3 =
            "D3" => Just (team "CRO" "Koratië")

        d4 =
            "D4" => Just (team "NGA" "Nigeria")

        e1 =
            "E1" => Just (team "BRA" "Brazilië")

        e2 =
            "E2" => Just (team "SUI" "Zwitserland")

        e3 =
            "E3" => Just (team "CRC" "Costa Rica")

        e4 =
            "E4" => Just (team "SRB" "Servië")

        f1 =
            "F1" => Just (team "GER" "Duitsland")

        f2 =
            "F2" => Just (team "MEX" "Mexico")

        f3 =
            "F3" => Just (team "SWE" "Zweden")

        f4 =
            "F4" => Just (team "KOR" "Zuid-Korea")

        g1 =
            "G1" => Just (team "BEL" "België")

        g2 =
            "G2" => Just (team "PAN" "Panama")

        g3 =
            "G3" => Just (team "TUN" "Tunesië")

        g4 =
            "G4" => Just (team "ENG" "Engeland")

        h1 =
            "H1" => Just (team "POL" "Polen")

        h2 =
            "H2" => Just (team "SEN" "Senegal")

        h3 =
            "H3" => Just (team "COL" "Colombia")

        h4 =
            "H4" => Just (team "JPN" "Japan")

        wa =
            "WA" => Nothing

        wb =
            "WB" => Nothing

        wc =
            "WC" => Nothing

        wd =
            "WD" => Nothing

        we =
            "WE" => Nothing

        wf =
            "WF" => Nothing

        wg =
            "WG" => Nothing

        wh =
            "WH" => Nothing

        ra =
            "RA" => Nothing

        rb =
            "RB" => Nothing

        rc =
            "RC" => Nothing

        rd =
            "RD" => Nothing

        re =
            "RE" => Nothing

        rf =
            "RF" => Nothing

        rg =
            "RG" => Nothing

        rh =
            "RH" => Nothing

        -- Quarter finalists
        w49 =
            "W49" => Nothing

        w50 =
            "W50" => Nothing

        w51 =
            "W51" => Nothing

        w52 =
            "W52" => Nothing

        w53 =
            "W53" => Nothing

        w54 =
            "W54" => Nothing

        w55 =
            "W55" => Nothing

        w56 =
            "W56" => Nothing

        -- Semi finalists
        w57 =
            "W57" => Nothing

        w58 =
            "W58" => Nothing

        w59 =
            "W59" => Nothing

        w60 =
            "W60" => Nothing

        -- Finalists
        w61 =
            "W61" => Nothing

        w62 =
            "W62" => Nothing

        -- 3rd
        w63 =
            "W63" => Nothing

        -- Champion
        w64 =
            "W64" => Nothing

        moskou =
            { town = "Moskou", stadium = "Luzhniki Stadion" }

        spartak =
            { town = "Moskou", stadium = "Spartak stadion" }

        petersburg =
            { town = "Sint-Petersburg", stadium = "Sint-Petersburg stadion" }

        novgorod =
            { town = "Nizhny Novgorod", stadium = "Nizhny Novgorod" }

        volgograd =
            { town = "Volgograd", stadium = "Volgograd Arena" }

        jekaterinenburg =
            { town = "Jekaterinenburg", stadium = "Jekaterinenburg Arena" }

        sotsji =
            { town = "Sotsji", stadium = "Fisht Stadion" }

        rostov =
            { town = "Rostov", stadium = "Rostov Arena" }

        saransk =
            { town = "Saransk", stadium = "Mordovia Arena" }

        samara =
            { town = "Samara", stadium = "Cosmos Arena" }

        kazan =
            { town = "Kazan", stadium = "Kazan Arena" }

        kaliningrad =
            { town = "Kaliningrad ", stadium = "Kaliningrad Stadion" }

        tnwa =
            TeamNode "WA" Nothing TBD

        tnwb =
            TeamNode "WB" Nothing TBD

        tnwc =
            TeamNode "WC" Nothing TBD

        tnwd =
            TeamNode "WD" Nothing TBD

        tnwe =
            TeamNode "WE" Nothing TBD

        tnwf =
            TeamNode "WF" Nothing TBD

        tnwg =
            TeamNode "WG" Nothing TBD

        tnwh =
            TeamNode "WH" Nothing TBD

        tnra =
            TeamNode "RA" Nothing TBD

        tnrb =
            TeamNode "RB" Nothing TBD

        tnrc =
            TeamNode "RC" Nothing TBD

        tnrd =
            TeamNode "RD" Nothing TBD

        tnre =
            TeamNode "RE" Nothing TBD

        tnrf =
            TeamNode "RF" Nothing TBD

        tnrg =
            TeamNode "RG" Nothing TBD

        tnrh =
            TeamNode "RH" Nothing TBD

        -- second round matches
        mn49 =
            MatchNode "m49" None tnwa tnrb II TBD

        mn50 =
            MatchNode "m50" None tnwc tnrd II TBD

        mn51 =
            MatchNode "m51" None tnwb tnra II TBD

        mn52 =
            MatchNode "m52" None tnwd tnrc II TBD

        mn53 =
            MatchNode "m53" None tnwe tnrf II TBD

        mn54 =
            MatchNode "m54" None tnwg tnrh II TBD

        mn55 =
            MatchNode "m55" None tnwf tnre II TBD

        mn56 =
            MatchNode "m56" None tnwh tnrg II TBD

        -- quarter finals
        mn57 =
            MatchNode "m57" None mn49 mn50 III TBD

        mn58 =
            MatchNode "m58" None mn53 mn54 III TBD

        mn59 =
            MatchNode "m59" None mn51 mn52 III TBD

        mn60 =
            MatchNode "m60" None mn55 mn56 III TBD

        -- semi finals
        mn61 =
            MatchNode "m61" None mn57 mn58 IV TBD

        mn62 =
            MatchNode "m62" None mn59 mn60 IV TBD

        -- finals
        mn64 =
            MatchNode "m64" None mn61 mn62 V TBD

        bracket =
            mn64
    in
        -- group A
        [ "m01" => answerGroupMatch A a1 a2 "2018/06/14 17:00" moskou
        , "m02" => answerGroupMatch A a3 a4 "2018/06/15 15:00" jekaterinenburg
        , "m17" => answerGroupMatch A a1 a3 "2018/06/19 20:00" petersburg
        , "m18" => answerGroupMatch A a4 a2 "2018/06/20 17:00" rostov
        , "m33" => answerGroupMatch A a4 a1 "2018/06/25 16:00" samara
        , "m34" => answerGroupMatch A a2 a3 "2018/06/25 16:00" volgograd
          -- group B
        , "m03" => answerGroupMatch B b1 b2 "2018/06/15 20:00" sotsji
        , "m04" => answerGroupMatch B b3 b4 "2018/06/15 17:00" petersburg
        , "m19" => answerGroupMatch B b1 b3 "2018/06/20 14:00" moskou
        , "m20" => answerGroupMatch B b4 b2 "2018/06/20 20:00" kazan
        , "m35" => answerGroupMatch B b4 b1 "2018/06/25 20:00" saransk
        , "m36" => answerGroupMatch B b2 b3 "2018/06/25 20:00" kaliningrad
          -- group C
        , "m05" => answerGroupMatch C c1 c2 "2018/06/16 12:00" kazan
        , "m06" => answerGroupMatch C c3 c4 "2018/06/16 18:00" saransk
        , "m22" => answerGroupMatch C c4 c2 "2018/06/21 14:00" samara
        , "m21" => answerGroupMatch C c1 c3 "2018/06/21 17:00" jekaterinenburg
        , "m37" => answerGroupMatch C c4 c1 "2018/06/26 16:00" moskou
        , "m38" => answerGroupMatch C c2 c3 "2018/06/26 16:00" sotsji
          -- group D
        , "m07" => answerGroupMatch D d1 d2 "2018/06/16 15:00" spartak
        , "m08" => answerGroupMatch D d3 d4 "2018/06/16 21:00" kaliningrad
        , "m23" => answerGroupMatch D d1 d3 "2018/06/21 20:00" novgorod
        , "m24" => answerGroupMatch D d4 d2 "2018/06/22 17:00" volgograd
        , "m39" => answerGroupMatch D d4 d1 "2018/06/26 20:00" petersburg
        , "m40" => answerGroupMatch D d2 d3 "2018/06/26 20:00" rostov
          -- group E
        , "m10" => answerGroupMatch E e3 e4 "2018/06/17 14:00" samara
        , "m09" => answerGroupMatch E e1 e2 "2018/06/17 20:00" rostov
        , "m25" => answerGroupMatch E e1 e3 "2018/06/22 14:00" petersburg
        , "m26" => answerGroupMatch E e4 e2 "2018/06/22 20:00" kaliningrad
        , "m41" => answerGroupMatch E e4 e1 "2018/06/27 20:00" spartak
        , "m42" => answerGroupMatch E e2 e3 "2018/06/27 20:00" novgorod
          -- group F
        , "m11" => answerGroupMatch F f1 f2 "2018/06/17 17:00" moskou
        , "m12" => answerGroupMatch F f3 f4 "2018/06/18 14:00" novgorod
        , "m28" => answerGroupMatch F f4 f2 "2018/06/23 17:00" rostov
        , "m27" => answerGroupMatch F f1 f3 "2018/06/23 20:00" sotsji
        , "m43" => answerGroupMatch F f4 f1 "2018/06/27 16:00" kazan
        , "m44" => answerGroupMatch F f2 f3 "2018/06/27 16:00" jekaterinenburg
          -- group G
        , "m13" => answerGroupMatch G g1 g2 "2018/06/18 17:00" sotsji
        , "m14" => answerGroupMatch G g3 g4 "2018/06/18 20:00" volgograd
        , "m29" => answerGroupMatch G g1 g3 "2018/06/23 14:00" spartak
        , "m30" => answerGroupMatch G g4 g2 "2018/06/24 14:00" novgorod
        , "m45" => answerGroupMatch G g4 g1 "2018/06/28 20:00" kaliningrad
        , "m46" => answerGroupMatch G g2 g4 "2018/06/28 20:00" saransk
          -- group H
        , "m16" => answerGroupMatch H h3 h4 "2018/06/19 14:00" saransk
        , "m15" => answerGroupMatch H h1 h2 "2018/06/19 17:00" spartak
        , "m32" => answerGroupMatch H h4 h2 "2018/06/24 17:00" jekaterinenburg
        , "m31" => answerGroupMatch H h1 h3 "2018/06/24 20:00" kazan
        , "m47" => answerGroupMatch H h4 h1 "2018/06/28 16:00" volgograd
        , "m48" => answerGroupMatch H h2 h3 "2018/06/28 16:00" samara
          -- Second rounders
        , "wa" => AnswerGroupPosition A First wa Nothing
        , "ra" => AnswerGroupPosition A Second ra Nothing
        , "wb" => AnswerGroupPosition B First wb Nothing
        , "rb" => AnswerGroupPosition B Second rb Nothing
        , "wc" => AnswerGroupPosition C First wc Nothing
        , "rc" => AnswerGroupPosition C Second rc Nothing
        , "wd" => AnswerGroupPosition D First wd Nothing
        , "rd" => AnswerGroupPosition D Second rd Nothing
        , "we" => AnswerGroupPosition E First we Nothing
        , "re" => AnswerGroupPosition E Second re Nothing
        , "wf" => AnswerGroupPosition F First wf Nothing
        , "rf" => AnswerGroupPosition F Second rf Nothing
        , "wg" => AnswerGroupPosition G First wg Nothing
        , "rg" => AnswerGroupPosition G Second rg Nothing
        , "wh" => AnswerGroupPosition H First wh Nothing
        , "rh" => AnswerGroupPosition H Second rh Nothing
          -- topscorer
        , "ts" => AnswerTopscorer ( Nothing, Nothing ) Nothing
          -- participant
        , "me" => AnswerParticipant (P.init)
          -- bracket
        , "br" => AnswerBracket bracket Nothing
        ]


answerGroupMatch : Group -> Draw -> Draw -> DateString -> Stadium -> AnswerT
answerGroupMatch group home away dateStr stadium =
    let
        date =
            toDate dateStr

        points =
            Nothing

        score =
            Nothing

        match =
            ( home, away, date, stadium )
    in
        AnswerGroupMatch group match score points


answerMatchWinnerInit : Round -> Draw -> Draw -> DateString -> Stadium -> Maybe DrawID -> AnswerT
answerMatchWinnerInit round home away dateStr stadium mNextId =
    let
        date =
            toDate dateStr

        points =
            Nothing

        team =
            Nothing

        match =
            ( home, away, date, stadium )
    in
        AnswerMatchWinner round match mNextId team points
