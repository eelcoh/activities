module UI.Team exposing (viewTeam, viewTeamEl, viewTeamHome, viewTeamAway, button)

import Bets.Types exposing (Team)
import Bets.Types.Team as T
import Element exposing (Element, column, el, image, layout, row)
import Element.Events exposing (onClick)
import Element.Attributes exposing (center, height, width, fill, padding, px, verticalCenter, verticalSpread, spread)
import Html exposing (Html, div, span, text)
import UI.Style exposing (..)
import Types exposing (Msg)


viewTeam : Maybe Team -> Html msg
viewTeam mTeam =
    layout stylesheet <| viewTeamEl mTeam


viewTeamEl : Maybe Team -> Element Style variation msg
viewTeamEl team =
    let
        teamName =
            Maybe.map T.display team
                |> Maybe.withDefault "..."

        img =
            { src = T.flagUrl team
            , caption =
                teamName
            }
    in
        column TeamBox
            [ verticalSpread, height (px 45), width (px 34) ]
            [ row Flag
                []
                [ image FlagImage [] img
                ]
            , row TeamName
                [ center ]
                [ Element.el UI.Style.TeamName [ center ] (Element.text (teamName)) ]
            ]


viewTeamHome : Maybe Team -> Element Style variation msg
viewTeamHome mTeam =
    let
        ( flag, team ) =
            viewTeamRows mTeam
    in
        viewTeamBox team flag


viewTeamAway : Maybe Team -> Element Style variation msg
viewTeamAway mTeam =
    let
        ( flag, team ) =
            viewTeamRows mTeam
    in
        viewTeamBox flag team


viewTeamBox : Element Style variation msg -> Element Style variation msg -> Element Style variation msg
viewTeamBox el1 el2 =
    row TeamBox
        [ spread, height (px 45), width (px 134) ]
        [ el1
        , el2
        ]


viewTeamRows : Maybe Team -> ( Element Style variation msg, Element Style variation msg )
viewTeamRows team =
    let
        teamName =
            Maybe.map T.display team
                |> Maybe.withDefault "..."

        img =
            { src = T.flagUrl team
            , caption =
                teamName
            }
    in
        ( row Flag
            [ center ]
            [ image FlagImage [] img
            ]
        , row TeamName
            [ center ]
            [ Element.el UI.Style.TeamName [ center ] (Element.text (teamName)) ]
        )


button :
    UI.Style.Qualified
    -> Bets.Types.Team
    -> Msg
    -> Element Style variation Msg
button semantics team msg =
    let
        w =
            width (px 64)

        h =
            height (px 76)

        buttonLayout =
            [ w, h, center, verticalCenter ]

        textElement =
            Element.el UI.Style.TeamName [ onClick msg ] (viewTeamEl (Just team))
    in
        Element.column (UI.Style.TeamButton semantics) buttonLayout [ textElement ]



-- viewTeamFull : Maybe Team -> Element Style variation msg
-- viewTeamFull team =
--     let
--         teamName =
--             Maybe.map T.displayFull team
--                 |> Maybe.withDefault "..."
--         img =
--             { src = T.flagUrl team
--             , caption =
--                 teamName
--             }
--         w =
--             Element.Attributes.width (px 150)
--         h =
--             Element.Attributes.height (px 82)
--     in
--         column TeamBox
--             [ verticalSpread, h, w, center, padding 10 ]
--             [ row Flag
--                 [ center ]
--                 [ image FlagImage [] img
--                 ]
--             , row TeamNameFull
--                 [ center ]
--                 [ Element.el UI.Style.TeamNameFull [ center ] (Element.text (teamName)) ]
--             ]
-- viewTeamLarge : Maybe Team -> Html msg
-- viewTeamLarge team =
--     div [ class "team" ]
--         [ span [ class "flag" ] [ T.flag team ]
--         , Html.br [] []
--         , span [ class "team-name-lg" ] [ text (T.mdisplayFull team) ]
--         ]
