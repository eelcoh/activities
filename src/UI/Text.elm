module UI.Text exposing (..)

import Element
import Element.Attributes exposing (center, alignLeft, px, spacing, paddingLeft, paddingTop, verticalCenter, maxWidth, width)
import UI.Style


header1 : String -> Element.Element UI.Style.Style variation msg
header1 txt =
    Element.header
        UI.Style.Header1
        [ paddingTop 50
        ]
        (Element.text txt)


header2 : String -> Element.Element UI.Style.Style variation msg
header2 txt =
    Element.header UI.Style.None
        [ maxWidth (px 600), spacing 7, paddingTop 50 ]
        (Element.paragraph
            UI.Style.Header2
            [ width Element.Attributes.fill ]
            [ (Element.text txt) ]
        )


simpleText : String -> Element.Element UI.Style.Style variation msg
simpleText txt =
    Element.el UI.Style.Text [] (Element.text txt)


bulletText : String -> Element.Element UI.Style.Style variation msg
bulletText txt =
    let
        bullet =
            Element.column UI.Style.None
                [ paddingTop 5 ]
                [ Element.circle 3 UI.Style.Bullet [ alignLeft ] Element.empty ]

        contents =
            Element.paragraph UI.Style.Introduction [ width Element.Attributes.fill ] [ (Element.text txt) ]
    in
        Element.row UI.Style.None [ paddingLeft 5, spacing 7 ] [ bullet, contents ]


boldText : String -> Element.Element UI.Style.Style variation msg
boldText txt =
    Element.el UI.Style.Emphasis [] (Element.text txt)


error : String -> Element.Element UI.Style.Style variation msg
error txt =
    Element.el
        UI.Style.Error
        []
        (Element.text txt)
