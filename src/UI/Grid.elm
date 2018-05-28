module UI.Grid
    exposing
        ( wrapper
        , container
        , row
        , cell
        , cell2
        , Size(..)
        , Color(..)
        , Style(..)
        , Align(..)
        , button
        , button2
        , pill
        , scoreButton
        , txt
        , score
        )

import Html exposing (Html, Attribute, div, span)
import Html.Attributes exposing (class)
import String


type Size
    = XXL
    | XL
    | L
    | M
    | S
    | XS
    | XXS
    | XXXS


type Color
    = Active
    | Inactive
    | Wrong
    | Right
    | Perhaps
    | Irrelevant
    | Potential
    | Selected


type Style
    = Big
    | Team


type Align
    = Leftside
    | Rightside
    | Center
    | Justified
    | Spaced


type alias Class =
    String


addclass : Class -> List (Attribute msg) -> List (Html msg) -> Html msg
addclass clstr attrs contents =
    let
        cls =
            class clstr
    in
        div (cls :: attrs) contents


toClasses : List Class -> Attribute msg
toClasses clss =
    String.join " " clss
        |> class


wrapper : List (Attribute msg) -> List (Html msg) -> Html msg
wrapper attrs contents =
    let
        cls =
            class "wrapper"
    in
        div (cls :: attrs) contents


container : Align -> List (Attribute msg) -> List (Html msg) -> Html msg
container align attrs contents =
    let
        cls =
            containerStyle align
    in
        div (cls :: attrs) contents


row : Align -> List (Attribute msg) -> List (Html msg) -> Html msg
row align attrs contents =
    let
        cls =
            rowStyle align
    in
        div (cls :: attrs) contents


containerStyle : Align -> Attribute msg
containerStyle align =
    class ("container " ++ (alignment align))


rowStyle : Align -> Attribute msg
rowStyle align =
    class ("row " ++ (alignment align))


alignment : Align -> String
alignment align =
    case align of
        Leftside ->
            "left"

        Rightside ->
            "rightside"

        Center ->
            "centered"

        Spaced ->
            "spaced"

        Justified ->
            "justified"


body : List (Attribute msg) -> List (Html msg) -> Html msg
body attrs contents =
    Html.body attrs contents


cell : Size -> Color -> List (Attribute msg) -> List (Html msg) -> Html msg
cell sz colr attrs contents =
    let
        classes =
            [ size sz
            , "cell"
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


cell2 : Size -> Color -> List (Attribute msg) -> List (Html msg) -> Html msg
cell2 sz colr attrs contents =
    let
        classes =
            [ size sz
            , "cell2"
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


toList : a -> List a
toList c =
    [ c ]


button : Size -> Color -> List (Attribute msg) -> List (Html msg) -> Html msg
button sz colr attrs contents =
    let
        classes =
            [ "cell"
            , "button"
            , "clickable"
            , size sz
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


button2 : Size -> Color -> List (Attribute msg) -> List (Html msg) -> Html msg
button2 sz colr attrs contents =
    let
        classes =
            [ "cell2"
            , "button"
            , "clickable"
            , size sz
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


pill : Color -> List (Attribute msg) -> List (Html msg) -> Html msg
pill colr attrs contents =
    let
        sz =
            XXXS

        classes =
            [ "cell2"
            , "button"
            , "clickable"
            , size sz
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


scoreButton : Color -> List (Attribute msg) -> List (Html msg) -> Html msg
scoreButton colr attrs contents =
    let
        classes =
            [ "cell2"
            , "button"
            , "clickable"
            , "scoreButton"
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


score : Size -> Color -> List (Attribute msg) -> List (Html msg) -> Html msg
score sz colr attrs contents =
    let
        classes =
            [ "cell"
            , "score"
            , "clickable"
            , size sz
            , color colr
            ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        div attributes contents


txt : List String -> Size -> Color -> List (Attribute msg) -> List (Html msg) -> Html msg
txt clss sz colr attrs contents =
    let
        classes =
            List.append
                clss
                [ size sz
                , color colr
                ]
                |> toClasses

        attributes =
            classes :: attrs
    in
        span attributes contents


size : Size -> Class
size sz =
    case sz of
        XXL ->
            "xxl"

        XL ->
            "xl"

        L ->
            "l"

        M ->
            "m"

        S ->
            "s"

        XS ->
            "xs"

        XXS ->
            "xxs"

        XXXS ->
            "xxxs"


styles : List Style -> List Class
styles stls =
    List.map toStyle stls


toStyle : Style -> Class
toStyle s =
    case s of
        Big ->
            "big"

        Team ->
            "team"


color : Color -> Class
color c =
    case c of
        Active ->
            "active"

        Inactive ->
            "inactive"

        Wrong ->
            "wrong"

        Right ->
            "right"

        Perhaps ->
            "perhaps"

        Irrelevant ->
            "irrelevant"

        Potential ->
            "potential"

        Selected ->
            "selected"



-- commentBox author comment date =
--     Element.column UI.Style.CommentBox
--         [ verticalSpread, h, w, center, padding 10 ]
--         [ row UI.Style.None
--             [ alignLeft ]
--             [ Element.el UI.Style.AuthorText [] (Element.text (author)) ]
--         , row UI.Style.None
--             [ alignLeft ]
--             [ Element.el UI.Style.CommentText [] (Element.text (comment)) ]
--         , row UI.Style.None
--             [ alignRight ]
--             [ Element.el UI.Style.DateText [] (Element.text (date)) ]
--         ]
