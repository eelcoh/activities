module Knockouts exposing (..)

import Types exposing (Model, Activity(..), Msg(..), Token(..), KnockoutsResults, Knockouts, Access(..), DataStatus(..))
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Json.Encode
import Element exposing (column, row)
import Element.Attributes exposing (spread, px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, spacingXY, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Style
import UI.Text
import UI.Team
import UI.Button
import Bets.Types exposing (Team, Round(..), HasQualified(..))
import Bets.Types.Team
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


updateKnockoutsResults : Token -> KnockoutsResults -> Cmd Msg
updateKnockoutsResults (Token token) results =
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
update rnd q team res =
    case res of
        Success results ->
            updateRes rnd q team (Debug.log "res" results)
                |> RemoteData.succeed

        _ ->
            res


updateRes : Round -> HasQualified -> Team -> KnockoutsResults -> KnockoutsResults
updateRes rnd q team res =
    case rnd of
        I ->
            res

        II ->
            { res | ii = (updateRound res.ii q team) }

        III ->
            { res | iii = (updateRound res.iii q team) }

        IV ->
            { res | iv = (updateRound res.iv q team) }

        V ->
            { res | v = (updateRound res.v q team) }

        VI ->
            { res | vi = (updateRound res.vi q team) }


updateRound : Knockouts -> HasQualified -> Team -> Knockouts
updateRound kos q team =
    case q of
        In ->
            let
                teamsIn =
                    addTeam kos.teamsIn team

                teamsOut =
                    removeTeam kos.teamsOut team
            in
                { kos | teamsIn = teamsIn, teamsOut = teamsOut }

        Out ->
            let
                teamsIn =
                    removeTeam kos.teamsIn team

                teamsOut =
                    addTeam kos.teamsOut team
            in
                { kos | teamsIn = (Debug.log "teamsIn" teamsIn), teamsOut = (Debug.log "teamsOut" teamsOut) }

        TBD ->
            let
                teamsIn =
                    removeTeam kos.teamsIn team

                teamsOut =
                    removeTeam kos.teamsOut team
            in
                { kos | teamsIn = teamsIn, teamsOut = teamsOut }


removeTeam : List Team -> Team -> List Team
removeTeam teams team =
    List.filter (\t -> t.teamID /= team.teamID) teams


addTeam : List Team -> Team -> List Team
addTeam teams team =
    (Debug.log "team" team) :: (Debug.log "teams" teams)



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


viewKnockoutsResults : Access -> KnockoutsResults -> Element.Element UI.Style.Style variation Msg
viewKnockoutsResults auth results =
    let
        rounds =
            [ II, III, IV, V, VI ]

        roundViews =
            List.map (viewKnockouts auth results) rounds
    in
        Element.column UI.Style.None
            [ spacing 10 ]
            roundViews



-- Json


viewKnockouts : Access -> KnockoutsResults -> Round -> Element.Element UI.Style.Style variation Msg
viewKnockouts auth results rnd =
    let
        ( candidatesFull, teamsIn, teamsOut ) =
            case rnd of
                II ->
                    ( results.i.teamsIn, results.ii.teamsIn, results.ii.teamsOut )

                III ->
                    ( results.ii.teamsIn, results.iii.teamsIn, results.iii.teamsOut )

                IV ->
                    ( results.iii.teamsIn, results.iv.teamsIn, results.iv.teamsOut )

                V ->
                    ( results.iv.teamsIn, results.v.teamsIn, results.v.teamsOut )

                VI ->
                    ( results.v.teamsIn, results.vi.teamsIn, results.vi.teamsOut )

                I ->
                    ( [], results.i.teamsIn, results.i.teamsOut )

        action q =
            case auth of
                Unauthorised ->
                    (\_ -> None)

                Authorised ->
                    Qualify rnd q

        candidates =
            let
                checked =
                    (List.map .teamID teamsIn) ++ (List.map .teamID teamsOut)
            in
                List.filter (\t -> not (List.member t.teamID checked)) candidatesFull

        roundHeader =
            UI.Text.header2 ("Ronde " ++ (Basics.toString rnd))

        cands =
            Element.wrappedRow UI.Style.None [ spacing 10 ] <|
                List.map (mkTeamButton UI.Style.NotYet (action In)) candidates

        ins =
            Element.wrappedRow UI.Style.None [ spacing 10 ] <|
                List.map (mkTeamButton UI.Style.Did (action Out)) teamsIn

        outs =
            Element.wrappedRow UI.Style.None [ spacing 10 ] <|
                List.map (mkTeamButton UI.Style.DidNot (action TBD)) teamsOut
    in
        case auth of
            Authorised ->
                Element.column UI.Style.None
                    []
                    [ roundHeader
                    , UI.Text.header3 ("Kandidaten")
                    , cands
                    , UI.Text.header3 ("Gekwalificeerd")
                    , ins
                    , UI.Text.header3 ("Uitgeschakeld")
                    , outs
                    ]

            Unauthorised ->
                Element.column UI.Style.None
                    []
                    [ roundHeader
                    , UI.Text.header3 ("Gekwalificeerd")
                    , ins
                    , UI.Text.header3 ("Uitgeschakeld")
                    , outs
                    ]


mkTeamButton : UI.Style.Qualified -> (Team -> Msg) -> Team -> Element.Element UI.Style.Style variation Msg
mkTeamButton sem msg team =
    UI.Team.button sem team (msg team)


encode : KnockoutsResults -> Json.Encode.Value
encode results =
    Json.Encode.object
        [ ( "i", encodeKnockouts results.i )
        , ( "ii", encodeKnockouts results.ii )
        , ( "iii", encodeKnockouts results.iii )
        , ( "iv", encodeKnockouts results.iv )
        , ( "v", encodeKnockouts results.v )
        , ( "vi", encodeKnockouts results.vi )
        ]


encodeKnockouts : Knockouts -> Json.Encode.Value
encodeKnockouts kos =
    Json.Encode.object
        [ ( "teamsIn", teamsEncode kos.teamsIn )
        , ( "teamsOut", teamsEncode kos.teamsOut )
        ]


teamsEncode : List Team -> Json.Encode.Value
teamsEncode teams =
    List.map Bets.Types.Team.encode teams
        |> Json.Encode.list


decode : Decoder KnockoutsResults
decode =
    Json.Decode.map6 KnockoutsResults
        (field "i" decodeKnockouts)
        (field "ii" decodeKnockouts)
        (field "iii" decodeKnockouts)
        (field "iv" decodeKnockouts)
        (field "v" decodeKnockouts)
        (field "vi" decodeKnockouts)


decodeKnockouts : Decoder Knockouts
decodeKnockouts =
    Json.Decode.map2 Knockouts
        (field "teamsIn" decodeTeamsList)
        (field "teamsOut" decodeTeamsList)


decodeTeamsList : Decoder (List Team)
decodeTeamsList =
    Json.Decode.list Bets.Types.Team.decode
