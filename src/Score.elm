module Score exposing (scores, mkScore, viewKeyboard)

import Element exposing (column, row)
import Element.Attributes exposing (spread, px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, alignLeft, verticalSpread, center, verticalCenter, alignRight, width, height)
import Element.Events as Events
import UI.Style
import UI.Button
import Types exposing (MatchResult, Msg)


viewKeyboard : MatchResult -> Element.Element UI.Style.Style variation Msg
viewKeyboard match =
    let
        toButton ( _, ( h, a, t ) ) =
            scoreButton UI.Style.SBPotential match h a t

        toRow scoreList =
            Element.row UI.Style.ScoreRow
                [ center, spacing 2, verticalCenter ]
                (List.map toButton scoreList)
    in
        Element.column UI.Style.ScoreColumn
            [ spacing 2 ]
            (List.map toRow scores)


scoreString : a -> b -> String
scoreString h a =
    List.foldr (++) "" [ " ", (toString h), "-", (toString a), " " ]


scoreButton : UI.Style.ScoreButtonSemantics -> MatchResult -> Int -> Int -> String -> Element.Element UI.Style.Style variation Msg
scoreButton c match home away t =
    let
        score =
            mkScore home away

        msg =
            Types.UpdateMatchResult { match | score = score }
    in
        UI.Button.scoreButton c msg t


mkScore h a =
    Just ( (Just h), (Just a) )


row0 : List ( Int, Int )
row0 =
    [ ( 5, 0 )
    , ( 4, 0 )
    , ( 3, 0 )
    , ( 2, 0 )
    , ( 1, 0 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 0, 2 )
    , ( 0, 3 )
    , ( 0, 4 )
    , ( 0, 5 )
    ]


row1 : List ( Int, Int )
row1 =
    [ ( 5, 1 )
    , ( 4, 1 )
    , ( 3, 1 )
    , ( 2, 1 )
    , ( 1, 1 )
    , ( 1, 2 )
    , ( 1, 3 )
    , ( 1, 4 )
    , ( 1, 5 )
    ]


row2 : List ( Int, Int )
row2 =
    [ ( 5, 2 )
    , ( 4, 2 )
    , ( 3, 2 )
    , ( 2, 2 )
    , ( 2, 3 )
    , ( 2, 4 )
    , ( 2, 5 )
    ]


row3 : List ( Int, Int )
row3 =
    [ ( 5, 3 )
    , ( 4, 3 )
    , ( 3, 3 )
    , ( 3, 4 )
    , ( 3, 5 )
    ]


row4 : List ( Int, Int )
row4 =
    [ ( 5, 4 )
    , ( 4, 4 )
    , ( 4, 5 )
    ]


row5 : List ( Int, Int )
row5 =
    [ ( 5, 5 )
    ]


row6 : List ( Int, Int )
row6 =
    [ ( 6, 0 )
    , ( 6, 1 )
    , ( 6, 2 )
    , ( 2, 6 )
    , ( 1, 6 )
    , ( 0, 6 )
    ]


row7 : List ( Int, Int )
row7 =
    [ ( 7, 0 )
    , ( 7, 1 )
    , ( 7, 2 )
    , ( 2, 7 )
    , ( 1, 7 )
    , ( 0, 7 )
    ]


row8 : List ( Int, Int )
row8 =
    [ ( 8, 0 )
    , ( 8, 1 )
    , ( 8, 2 )
    , ( 2, 8 )
    , ( 1, 8 )
    , ( 0, 8 )
    ]


row9 : List ( Int, Int )
row9 =
    [ ( 9, 0 )
    , ( 9, 1 )
    , ( 9, 2 )
    , ( 2, 9 )
    , ( 1, 9 )
    , ( 0, 9 )
    ]


row10 : List ( Int, Int )
row10 =
    [ ( 10, 0 )
    , ( 10, 1 )
    , ( 10, 2 )
    , ( 2, 10 )
    , ( 1, 10 )
    , ( 0, 10 )
    ]


scores : List (List ( Int, ( Int, Int, String ) ))
scores =
    [ row0
    , row1
    , row2
    , row3
    , row4
    , row5
    , row6
    , row7
    , row8
    , row9
    , row10
    ]
        |> List.map indexedScores


indexedScores : List ( Int, Int ) -> List ( Int, ( Int, Int, String ) )
indexedScores scoreList =
    scoreList
        |> List.map (\( h, a ) -> ( h, a, (scoreString h a) ))
        |> List.indexedMap (,)
