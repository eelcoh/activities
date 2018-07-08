module Knockouts exposing (..)

import Types exposing (Model, Activity(..), Msg(..), Token(..), KnockoutsResults, TeamRounds, Access(..), DataStatus(..), Qualified(..))
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Json.Encode
import Element exposing (column, row)
import Element.Attributes exposing (spread, px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, spacingXY, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Text
import UI.Team
import UI.Button
import UI.Style
import Bets.Types exposing (Team, Round(..), HasQualified(..))
import Bets.Types.Team
import Bets.Types.Bracket
import Bets.Types.Round exposing (isSameOrANextRound)
import Bets.Types.HasQualified
import Http
import RemoteData exposing (WebData)


fetchKnockoutsResults : Cmd Msg
fetchKnockoutsResults =
    Web.get "/bets/results/knockouts/" FetchedKnockoutsResults decode


inititaliseKnockoutsResults : Token -> Cmd Msg
inititaliseKnockoutsResults (Token token) =
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
            "/bets/results/knockouts/initial/"

        json =
            Json.Encode.object []
    in
        Web.postWithConfig config url FetchedKnockoutsResults decode json


storeKnockoutsResults : Token -> KnockoutsResults -> Cmd Msg
storeKnockoutsResults (Token token) results =
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
            "/bets/results/knockouts/"

        json =
            encode results
    in
        Web.postWithConfig config url StoredKnockoutsResults decode json



-- update


update : Round -> HasQualified -> Team -> WebData KnockoutsResults -> WebData KnockoutsResults
update round qualified team results =
    case results of
        Success results ->
            updateKnockoutsResults round qualified team results
                |> RemoteData.succeed

        _ ->
            results


updateKnockoutsResults : Round -> HasQualified -> Team -> KnockoutsResults -> KnockoutsResults
updateKnockoutsResults round qualified team results =
    let
        teamResults =
            List.map (updateTeamRounds round qualified team) results.teams
    in
        { results | teams = teamResults }


updateTeamRounds : Round -> HasQualified -> Team -> ( String, TeamRounds ) -> ( String, TeamRounds )
updateTeamRounds round qualified team ( teamID, teamRounds ) =
    if team.teamID == teamID then
        let
            roundsQualified =
                updateRoundsQualified round qualified teamRounds.roundsQualified
        in
            ( teamID, { teamRounds | roundsQualified = roundsQualified } )
    else
        ( teamID, teamRounds )


updateRoundsQualified : Round -> HasQualified -> List ( Round, HasQualified ) -> List ( Round, HasQualified )
updateRoundsQualified round qualified teamRounds =
    List.map (updateRoundQualified round qualified) teamRounds


updateRoundQualified : Round -> HasQualified -> ( Round, HasQualified ) -> ( Round, HasQualified )
updateRoundQualified round qualified ( r, q ) =
    let
        cmp =
            Bets.Types.Round.compareRounds round r
    in
        case ( qualified, cmp ) of
            ( In, LT ) ->
                ( r, TBD )

            ( In, GT ) ->
                ( r, In )

            ( TBD, LT ) ->
                ( r, TBD )

            ( TBD, GT ) ->
                ( r, q )

            ( Out, LT ) ->
                ( r, Out )

            ( Out, GT ) ->
                ( r, q )

            ( _, EQ ) ->
                ( r, qualified )



-- View


view : Model -> Element.Element UI.Style.Style variation Msg
view model =
    let
        auth =
            case model.token of
                Success _ ->
                    Authorised

                _ ->
                    Unauthorised

        items =
            case ( auth, model.knockoutsResults ) of
                ( Authorised, Fresh (Success results) ) ->
                    [ viewKnockoutsResults auth results
                    , UI.Button.pill UI.Style.Inactive UpdateKnockoutsResults "Update"
                    , UI.Button.pill UI.Style.Potential InitialiseKnockoutsResults "Initialiseer"
                    ]

                ( Authorised, Dirty (Success results) ) ->
                    [ viewKnockoutsResults auth results
                    , UI.Button.pill UI.Style.Active UpdateKnockoutsResults "Update"
                    , UI.Button.pill UI.Style.Potential InitialiseKnockoutsResults "Initialiseer"
                    ]

                ( Authorised, _ ) ->
                    [ Element.text "Nog niet bekend"
                    , UI.Button.pill UI.Style.Inactive UpdateKnockoutsResults "Update"
                    , UI.Button.pill UI.Style.Potential InitialiseKnockoutsResults "Initialiseer"
                    ]

                ( Unauthorised, Fresh (Success results) ) ->
                    [ viewKnockoutsResults auth results ]

                ( _, _ ) ->
                    [ Element.text "..." ]
    in
        Element.column UI.Style.None
            [ spacingXY 0 14 ]
            items



-- {teams : List (TeamId, { team: Team, roundsQualified : List (Round, HasQualified)})}


viewKnockoutsResults : Access -> KnockoutsResults -> Element.Element UI.Style.Style variation Msg
viewKnockoutsResults auth results =
    let
        viewTeamRounds ( _, teamRounds ) =
            viewKnockoutsPerTeam teamRounds
    in
        Element.column UI.Style.None
            [ spacingXY 0 20 ]
            (List.map (Tuple.second >> viewKnockoutsPerTeam) results.teams)


viewKnockoutsPerTeam : TeamRounds -> Element.Element UI.Style.Style variation Msg
viewKnockoutsPerTeam { team, roundsQualified } =
    let
        teamBtn =
            UI.Team.badge NotYet team

        roundButtons =
            List.reverse roundsQualified
                |> List.map (viewRoundButtons team)
    in
        Element.row UI.Style.None
            [ padding 20, spacing 20 ]
            (teamBtn :: roundButtons)


viewRoundButtons : Team -> ( Round, HasQualified ) -> Element.Element UI.Style.Style variation Msg
viewRoundButtons team ( rnd, qualified ) =
    let
        succeeded =
            ChangeQualify rnd In team

        failed =
            ChangeQualify rnd Out team

        unknown =
            ChangeQualify rnd TBD team

        btnSemantic q =
            let
                h =
                    toHasQualified qualified
            in
                if q == h then
                    UI.Style.Selected
                else
                    UI.Style.Potential

        toHasQualified q =
            case q of
                In ->
                    Did

                Out ->
                    DidNot

                TBD ->
                    NotYet
    in
        Element.column UI.Style.None
            [ spacing 20 ]
            [ Element.el UI.Style.RankingHeader [ width (px 40) ] (Element.text (Basics.toString rnd))
            , UI.Button.pill (btnSemantic Did) succeeded "In"
            , UI.Button.pill (btnSemantic NotYet) unknown "TBD"
            , UI.Button.pill (btnSemantic DidNot) failed "Out"
            ]


mkTeamButton : Types.Qualified -> (Team -> Msg) -> Team -> Element.Element UI.Style.Style variation Msg
mkTeamButton sem msg team =
    UI.Team.button sem team (msg team)


encode : KnockoutsResults -> Json.Encode.Value
encode results =
    Json.Encode.object
        [ ( "teams", Json.Encode.object (List.map encodeTeamQ results.teams) )
        ]



-- encodeTeamQs : List ( String, TeamRounds ) -> Json.Encode.Value
-- encodeTeamQs ( teamId, teamrounds ) =
--     Json.Encode.list encodeTeamQ


encodeTeamQ : ( String, TeamRounds ) -> ( String, Json.Encode.Value )
encodeTeamQ ( teamId, teamrounds ) =
    ( teamId, encodeTeamRounds teamrounds )


encodeTeamRounds : TeamRounds -> Json.Encode.Value
encodeTeamRounds teamrounds =
    Json.Encode.object
        [ ( "team", Bets.Types.Team.encode teamrounds.team )
        , ( "roundsQualified", encodeRoundSQualified teamrounds.roundsQualified )
        ]


encodeRoundSQualified : List ( Round, HasQualified ) -> Json.Encode.Value
encodeRoundSQualified teamrounds =
    List.map roundQualifiedToString teamrounds
        |> Json.Encode.object


roundQualifiedToString ( r, rQ ) =
    ( Basics.toString r, Json.Encode.string (Basics.toString rQ) )



-- encodeKnockouts : Knockouts -> Json.Encode.Value
-- encodeKnockouts kos =
--     Json.Encode.object
--         [ ( "teamsIn", teamsEncode kos.teamsIn )
--         , ( "teamsOut", teamsEncode kos.teamsOut )
--         ]
-- teamsEncode : List Team -> Json.Encode.Value
-- teamsEncode teams =
--     List.map Bets.Types.Team.encode teams
--         |> Json.Encode.list


decode : Decoder KnockoutsResults
decode =
    Json.Decode.map KnockoutsResults
        (field "teams" (Json.Decode.keyValuePairs decodeTeamRounds))


decodeTeamRounds : Decoder TeamRounds
decodeTeamRounds =
    Json.Decode.map2 TeamRounds
        (field "team" Bets.Types.Team.decode)
        (field "roundsQualified" (decodeRoundsQualified))


decodeRoundsQualified : Decoder (List ( Round, HasQualified ))
decodeRoundsQualified =
    Json.Decode.keyValuePairs Bets.Types.HasQualified.decode
        |> Json.Decode.map mkRoundsQualified


mkRoundsQualified : List ( String, HasQualified ) -> List ( Round, HasQualified )
mkRoundsQualified list =
    List.filterMap parseRoundQualifiedPair list


parseRoundQualifiedPair : ( String, HasQualified ) -> Maybe ( Round, HasQualified )
parseRoundQualifiedPair ( rStr, hasQ ) =
    Bets.Types.Round.stringToRound rStr
        |> Maybe.map (\r -> ( r, hasQ ))



-- decoder : Decoder (List (Foo, Bar))
-- decoder =
--   keyValuePairs barDecoder
--     |> Decode.map parseKeys
-- NORMAL FUNCTIONS BELOW
-- parseKeys : List (String, Bar) -> List (Foo, Bar)
-- parseKeys list =
--   List.filterMap parseKVPair list
-- parseKVPair : (String, Bar) -> Maybe (Foo, Bar)
-- parseKVPair (str, value) =
--   parseKey str
--     |> Maybe.map (\key -> (key, value))
-- parseKey : String -> Maybe Foo
-- parseKey str =
--   case str of
--     "afoo" -> Just AFoo
--     "bfoo" -> Just BFoo
--     _ -> Nothing
-- Decoder (List (String, Bar)) -> Decoder (List (Foo, Bar))
-- decodeRoundQualified : Decoder ( Round, HasQualified )
-- decodeRoundQualified =
--     decodeRound
--         |> Json.Decode.andThen decodeRoundHasQualified
-- decodeRoundHasQualified : Round -> Decoder ( Round, HasQualified )
-- decodeRoundHasQualified r =
--     Bets.Types.Bracket.decodeHasQualified
--         |> Json.Decode.andThen (decodeRoundHasQualified2 r)
-- decodeRoundHasQualified2 : Round -> HasQualified -> Decoder ( Round, HasQualified )
-- decodeRoundHasQualified2 r q =
--     Json.Decode.succeed ( r, q )


decodeRound : String -> Decoder Round
decodeRound str =
    case str of
        "I" ->
            Json.Decode.succeed I

        "II" ->
            Json.Decode.succeed II

        "III" ->
            Json.Decode.succeed III

        "IV" ->
            Json.Decode.succeed IV

        "V" ->
            Json.Decode.succeed V

        "VI" ->
            Json.Decode.succeed VI

        s ->
            Json.Decode.fail ("Expected a Round, got : " ++ s)



-- decode : Decoder KnockoutsResults
-- decode =
--     Json.Decode.map6 KnockoutsResults
--         (field "i" decodeKnockouts)
--         (field "ii" decodeKnockouts)
--         (field "iii" decodeKnockouts)
--         (field "iv" decodeKnockouts)
--         (field "v" decodeKnockouts)
--         (field "vi" decodeKnockouts)
-- decodeKnockouts : Decoder Knockouts
-- decodeKnockouts =
--     Json.Decode.map2 Knockouts
--         (field "teamsIn" decodeTeamsList)
--         (field "teamsOut" decodeTeamsList)
-- decodeTeamsList : Decoder (List Team)
-- decodeTeamsList =
--     Json.Decode.list Bets.Types.Team.decode
