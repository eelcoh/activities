module Results exposing (..)

import Types exposing (Model, Activity(..), Msg(..), Token(..), MatchResults, MatchResult, Access(..))
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Json.Encode
import Element exposing (column, row)
import Element.Attributes exposing (spread, px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, alignLeft, verticalSpread, center, alignRight, width, height)
import Element.Events as Events
import UI.Style
import UI.Text
import UI.Team
import UI.Button
import Bets.Types
import Bets.Types.Team
import Bets.View
import Score
import Http


fetchMatchResults : Cmd Msg
fetchMatchResults =
    Web.get "/bets/results/matches/" FetchedMatchResults decode


updateMatchResults : Token -> MatchResult -> Cmd Msg
updateMatchResults (Token token) match =
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

        url =
            "/bets/results/matches/" ++ match.match

        json =
            encodeMatchResult match
    in
        Web.putWithConfig config url StoredMatchResult decodeMatchResult json


view : Model -> Element.Element UI.Style.Style variation Msg
view model =
    let
        items =
            case model.token of
                Success _ ->
                    [ viewMatchResults Authorised model.matchResults
                    ]

                _ ->
                    [ viewMatchResults Unauthorised model.matchResults ]
    in
        Element.column UI.Style.None
            []
            items


viewMatchResults : Access -> RemoteData.WebData MatchResults -> Element.Element UI.Style.Style variation Msg
viewMatchResults access results =
    case results of
        Success results ->
            displayMatches access results.results

        NotAsked ->
            Element.text "nog niet opgevraagd"

        Loading ->
            Element.text "aan het ophalen..."

        Failure e ->
            Element.column UI.Style.None
                []
                [ UI.Text.error "oei, oei, oei, daar ging iets niet helemaal goed"
                , Element.paragraph UI.Style.Text [] [ Element.text (Basics.toString e) ]
                ]


displayMatches : Access -> List MatchResult -> Element.Element UI.Style.Style variation Msg
displayMatches access matches =
    Element.wrappedRow UI.Style.Matches
        [ padding 10, spacing 7, center ]
        (List.map (displayMatch access) matches)


displayMatch : Access -> MatchResult -> Element.Element UI.Style.Style variation Msg
displayMatch access match =
    let
        home =
            UI.Team.viewTeamEl match.homeTeam

        away =
            UI.Team.viewTeamEl match.awayTeam

        sc =
            Bets.View.displayScore match.score

        pts =
            case match.score of
                Just _ ->
                    Just 3

                Nothing ->
                    Nothing

        click =
            case access of
                Authorised ->
                    Events.onClick (EditMatch match)

                Unauthorised ->
                    Events.onClick None
    in
        Element.row (UI.Style.MatchRow pts)
            [ spread, Element.Attributes.verticalCenter, click, paddingXY 10 5, spacing 7, width (px 150), height (px 70) ]
            [ home, sc, away ]



-- edit


edit : Model -> Element.Element UI.Style.Style variation Msg
edit model =
    let
        d =
            Debug.log "edit"

        items =
            case ( model.token, model.matchResult ) of
                ( Success _, Success match ) ->
                    [ displayMatch Unauthorised match
                    , Score.viewKeyboard match
                    , UI.Button.pill UI.Style.Active (CancelMatchResult match) "Wissen"
                    ]

                ( Success _, Failure e ) ->
                    [ UI.Text.error (Basics.toString e)
                    ]

                ( Success _, NotAsked ) ->
                    [ UI.Text.error "geen wedstrijd geselecteerd" ]

                _ ->
                    [ UI.Text.error "dit is niet de bedoeling" ]
    in
        Element.column UI.Style.None
            []
            items



-- json


encodeMatchResult : MatchResult -> Json.Encode.Value
encodeMatchResult match =
    let
        ( homeScore, awayScore, isSet ) =
            case match.score of
                Just ( Just h, Just a ) ->
                    ( h, a, True )

                Just ( Just h, Nothing ) ->
                    ( h, 0, False )

                Just ( Nothing, Just a ) ->
                    ( 0, a, False )

                Just ( Nothing, Nothing ) ->
                    ( 0, 0, False )

                Nothing ->
                    ( 0, 0, False )
    in
        Json.Encode.object
            [ ( "matchResultId", Json.Encode.string match.matchResultId )
            , ( "match", Json.Encode.string match.match )
            , ( "homeTeam", Bets.Types.Team.encode match.homeTeam )
            , ( "awayTeam", Bets.Types.Team.encode match.awayTeam )
            , ( "homeScore", Json.Encode.int homeScore )
            , ( "awayScore", Json.Encode.int awayScore )
            , ( "isSet", Json.Encode.bool isSet )
            ]


decode : Decoder MatchResults
decode =
    Json.Decode.map MatchResults
        (field "matchResults" (Json.Decode.list decodeMatchResult))


decodeMatchResult : Decoder MatchResult
decodeMatchResult =
    field "isSet" Json.Decode.bool
        |> andThen decodeScore
        |> andThen decodeRest


decodeScore : Bool -> Decoder (Maybe ( Maybe Int, Maybe Int ))
decodeScore isSet =
    if isSet then
        Json.Decode.map2 Score.mkScore
            (field "homeScore" Json.Decode.int)
            (field "awayScore" Json.Decode.int)
    else
        Json.Decode.map2 (\_ _ -> Nothing)
            (field "homeScore" Json.Decode.int)
            (field "awayScore" Json.Decode.int)


decodeRest : Maybe Bets.Types.Score -> Decoder MatchResult
decodeRest score =
    Json.Decode.map4 (\mResId m h a -> MatchResult mResId m h a score)
        (field "matchResultId" Json.Decode.string)
        (field "match" Json.Decode.string)
        (field "homeTeam" (Bets.Types.Team.decode))
        (field "awayTeam" (Bets.Types.Team.decode))
