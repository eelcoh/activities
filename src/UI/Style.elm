module UI.Style exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Color as Color
import Element.Font as Font
import Element.Scale as Scale
import Types exposing (Qualified(..), ScreenSize(..))
import UI.Color exposing (..)


type Style
    = Button ButtonSemantics
    | Introduction
    | Text
    | Title ScreenSize
    | Header1
    | Header2
    | Header3
    | Error
    | ErrorText
    | Page
    | Emphasis
    | None
    | TeamNameFull
    | Flag
    | FlagImage
    | TeamName
    | TeamBox
    | TeamButton Qualified
    | Matches
    | MatchRow (Maybe Int)
    | Score
    | ScoreRow
    | ScoreColumn
    | ScoreInput
    | Bullet
    | TextInput
    | TrapInput
    | Link
    | PostBox
    | CommentBox
    | AuthorText
    | CommentText
    | PostTitle
    | PostText
    | DateText
    | Menu
    | Centered
    | CommentInputBox
    | PostInputBox
    | AuthorInput
    | Clickable
    | NavLink ButtonSemantics
    | RankingGroup
    | RankingLine
    | RankingPoints
    | RankingPos
    | RankingName
    | RankingPointsH
    | RankingPosH
    | RankingNameH
    | RankingHeader
    | ScoreButton ScoreButtonSemantics


type ScoreButtonSemantics
    = SBPotential
    | SBSelected


type TeamButtonSemantics
    = TBPotential
    | TBSelected
    | TBInactive


type ButtonSemantics
    = Active
    | Inactive
    | Wrong
    | Right
    | Perhaps
    | Irrelevant
    | Potential
    | Selected
    | Trap


fontSansSerif : Property class variation
fontSansSerif =
    Font.typeface
        [ Font.font "Asap"
        , Font.font "Helvetica"
        , Font.font "Arial"
        , Font.sansSerif
        ]


fontSerif : Property class variation
fontSerif =
    Font.typeface
        [ Font.font "Lora"
        , Font.font "Georgia"
        , Font.serif
        ]


fontMono : Property class variation
fontMono =
    Font.typeface
        [ Font.font "Roboto Mono"
        , Font.monospace
        ]


scale : Int -> Float
scale =
    Scale.modular 16 1.618


stylesheet : StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ style (Title Small)
            [ Font.size 36
            , Color.text primaryText
            , Color.background primary
            , fontSansSerif
            ]
        , style (Title Big)
            [ Font.size 80
            , Color.text primaryText
            , Color.background primary
            , fontSansSerif
            ]
        , style Header1
            [ Font.size 36
            , Color.text secondaryText
            , fontSansSerif
            ]
        , style Header2
            [ Font.size 28
            , Color.text secondaryText
            , fontSansSerif
            ]
        , style Header3
            [ Font.size 20
            , Color.text secondaryText
            , fontSerif
            , Font.weight 600
            ]
        , style Menu
            [ Font.size 20
            , Color.text primaryText
            , Color.background primary
            , fontSansSerif
            ]
        , style Error
            [ Color.text red
            , Color.background secondaryLight
            , Border.all 1
            , Color.border red
            , Font.size 20
            , Font.lineHeight 1.4
            , fontSerif
            ]
        , style Text
            [ Font.size 20
            , Font.lineHeight 1.6
            , fontSerif
            ]
        , style Introduction
            [ Font.size 20
            , Font.lineHeight 1.4
            , fontSerif
            ]
        , style Page
            []
        , style (Button Active)
            [ Color.background primaryDark
            , Color.text primaryText
            , Border.all 2
            , hover
                [ cursor "pointer"
                , Color.background UI.Color.orange
                , Color.text UI.Color.white
                ]
            , fontMono
            ]
        , style (Button Inactive)
            [ Color.background secondary
            , Color.text secondaryDark
            , Font.lineHeight 1.0
            , Border.all 2
            , hover
                [ cursor "not-allowed" ]
            , fontMono
            ]
        , style (Button Wrong)
            [ Color.background <| rgb 233 30 99
            , Color.text UI.Color.white
            , Border.all 2
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (Button Right)
            [ Color.background selected
            , Color.text UI.Color.white
            , Border.all 2
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (Button Perhaps)
            [ Color.background selected
            , Color.text UI.Color.white
            , Border.all 2
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (Button Irrelevant)
            [ Color.background UI.Color.white
            , Color.text secondaryText
            , Border.all 2
            , Color.border primary
            , fontMono

            -- , hover [ cursor "pointer" ]
            ]
        , style (Button Potential)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 2
            , Color.border secondaryLight
            , hover
                [ cursor "pointer"
                , Border.all 2
                , Border.solid
                , Color.border black
                , Color.background UI.Color.white
                , Color.text black
                ]
            , fontMono
            ]
        , style (Button Selected)
            [ Color.background selected
            , Color.text UI.Color.white
            , Border.all 2
            , Color.border selected
            , hover
                [ cursor "pointer"
                , Color.background UI.Color.orange
                , Color.text black
                , Color.border UI.Color.orange
                ]
            , fontMono
            ]
        , style (Button Trap)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.alignLeft
            , Border.all 2
            , hover
                [ cursor "cursor"
                , Color.background secondary
                ]
            ]
        , style (TeamButton Did)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 5
            , Color.border UI.Color.right
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (TeamButton DidNot)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 5
            , Color.border UI.Color.wrong
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (TeamButton NotYet)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 5
            , Color.border secondaryLight
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (MatchRow Nothing)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , Border.all 5
            , Color.border secondaryLight
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (MatchRow (Just 1))
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , Border.all 5
            , Border.dashed
            , Color.border UI.Color.right
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (MatchRow (Just 3))
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , Border.all 5
            , Color.border UI.Color.right
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style (MatchRow (Just 0))
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , Border.all 5
            , Color.border UI.Color.wrong
            , hover [ cursor "pointer" ]
            , fontMono
            ]
        , style Emphasis
            [ Color.text UI.Color.orange
            , Font.weight 700
            ]
        , style Flag
            []
        , style FlagImage
            []
        , style TeamName
            [ Font.center
            ]
        , style TeamBox
            []
        , style Matches
            []
        , style ScoreRow
            [ Border.bottom 1
            , Color.border secondary
            , fontMono
            ]
        , style ScoreColumn
            []
        , style ScoreInput
            []
        , style Score
            [ Font.center
            , fontMono
            ]
        , style None
            []
        , style Centered
            [ Font.center
            ]
        , style Bullet
            [ Color.background UI.Color.orange ]
        , style TextInput
            [ Border.all 2
            , Color.border secondaryLight
            , Color.placeholder secondaryDark
            , Font.size 20
            ]
        , style TrapInput
            [ Border.all 2
            , Color.border secondaryLight
            , Color.placeholder secondaryDark
            , Font.size 20
            ]
        , style Link
            [ Color.text UI.Color.secondaryText
            , fontSerif
            , Font.size 20
            , Font.underline
            , hover
                [ cursor "pointer"
                , Color.background secondaryLight
                ]
            ]
        , style CommentBox
            [ Color.border secondaryLight
            , Border.bottom 1
            , fontSerif
            , Font.size 20
            ]
        , style PostBox
            [ Color.background secondaryLight
            , fontSerif
            , Font.size 20
            ]
        , style CommentInputBox
            [ Color.background secondaryLight
            , Color.placeholder secondary
            , fontSerif
            , Font.size 20
            ]
        , style PostInputBox
            [ Color.background secondaryDark
            , Color.placeholder secondary
            , fontSerif
            , Font.size 20
            ]
        , style (NavLink Selected)
            [ Border.bottom 8
            , Color.border secondary
            , Style.hover
                [ Border.bottom 8
                , Color.border primary
                ]
            ]
        , style (NavLink Potential)
            [ Border.bottom 8
            , Color.border primary
            , Style.hover
                [ Border.bottom 8
                , Color.border secondary
                ]
            ]
        , style Clickable
            [ Font.underline
            , Color.text secondaryText
            , Font.weight 700
            , Color.background primaryDark
            ]
        , style AuthorText
            [ Color.text secondaryText
            , Font.size 12
            , Font.center
            , fontSansSerif
            ]
        , style PostText
            [ Color.text secondaryText
            , Font.size 26
            , Font.alignLeft
            , Font.lineHeight 1.4
            , fontSerif
            ]
        , style PostTitle
            [ Color.text secondaryText
            , Font.size 32
            , Font.alignLeft
            , fontSansSerif
            ]
        , style CommentText
            [ Color.text secondaryText
            , Font.size 26
            , Font.lineHeight 1.4
            , Font.alignLeft
            , fontSerif
            ]
        , style DateText
            [ Color.text secondaryText
            , Font.size 12
            , Font.center
            , fontSansSerif
            ]
        , style RankingGroup
            [ Border.bottom 1
            , Color.border secondary
            , fontMono
            ]
        , style RankingHeader
            [ Border.bottom 2
            , Color.border secondary
            , fontMono
            ]
        , style RankingPoints
            [ fontMono
            , Font.alignRight
            ]
        , style RankingPos
            [ fontMono
            , Font.alignRight
            ]
        , style RankingName
            [ fontMono
            , Font.alignLeft
            , hover
                [ cursor "pointer"
                , Color.background secondaryLight
                , Color.text UI.Color.orange
                , Font.weight 700
                ]
            ]
        , style RankingPosH
            [ fontMono
            , Font.alignRight
            , Font.weight 700
            ]
        , style RankingNameH
            [ fontMono
            , Font.alignLeft
            , Font.weight 700
            ]
        , style RankingPointsH
            [ fontMono
            , Font.alignRight
            , Font.weight 700
            ]
        , style (ScoreButton SBPotential)
            [ Color.background secondary
            , Color.text secondaryText
            , Border.all 1
            , Color.border secondary
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (ScoreButton SBSelected)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 1
            , Color.border secondary
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        ]
