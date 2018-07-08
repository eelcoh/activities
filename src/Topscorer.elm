module Topscorer exposing (..)

import Types exposing (Model, Activity(..), Msg(..), Token(..), TopscorerResults, Topscorer, Access(..), DataStatus(..))
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Json.Encode
import Element exposing (column, row)
import Element.Attributes exposing (spread, px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, spacingXY, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Button
import UI.Style
import Bets.Types exposing (Team, HasQualified(..))
import Bets.Types.Team
import Bets.Types.HasQualified
import Http
import RemoteData exposing (WebData)


fetchTopscorerResults : Cmd Msg
fetchTopscorerResults =
    Web.get "/bets/results/topscorer/" FetchedTopscorerResults decode


inititaliseTopscorerResults : Token -> Cmd Msg
inititaliseTopscorerResults (Token token) =
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
            "/bets/results/topscorer/initial/"

        json =
            Json.Encode.object []
    in
        Web.postWithConfig config url FetchedTopscorerResults decode json


storeTopscorerResults : Token -> TopscorerResults -> Cmd Msg
storeTopscorerResults (Token token) results =
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
            "/bets/results/topscorer/"

        json =
            encode results
    in
        Web.postWithConfig config url StoredTopscorerResults decode json



-- update


update : HasQualified -> Topscorer -> WebData TopscorerResults -> WebData TopscorerResults
update qualified topscorer results =
    case results of
        Success results ->
            updateTopscorerResults qualified topscorer results
                |> RemoteData.succeed

        _ ->
            results


updateTopscorerResults : HasQualified -> Topscorer -> TopscorerResults -> TopscorerResults
updateTopscorerResults qualified topscorer results =
    let
        topscorerResults =
            List.map (updateTopscorer qualified topscorer) results.topscorers
    in
        { results | topscorers = topscorerResults }


updateTopscorer : HasQualified -> Topscorer -> ( HasQualified, Topscorer ) -> ( HasQualified, Topscorer )
updateTopscorer qualified ts ( hasQ, topscorer ) =
    if (topscorer.team.teamID == ts.team.teamID) && (topscorer.topscorer == ts.topscorer) then
        ( qualified, topscorer )
    else
        ( hasQ, topscorer )



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
            case ( auth, model.topscorerResults ) of
                ( Authorised, Fresh (Success results) ) ->
                    [ UI.Button.pill UI.Style.Inactive UpdateTopscorerResults "Update"
                    , viewTopscorerResults auth results
                    , UI.Button.pill UI.Style.Potential InitialiseTopscorerResults "Initialiseer"
                    ]

                ( Authorised, Dirty (Success results) ) ->
                    [ UI.Button.pill UI.Style.Active UpdateTopscorerResults "Update"
                    , viewTopscorerResults auth results
                    , UI.Button.pill UI.Style.Potential InitialiseTopscorerResults "Initialiseer"
                    ]

                ( Authorised, _ ) ->
                    [ UI.Button.pill UI.Style.Inactive UpdateTopscorerResults "Update"
                    , Element.text "Nog niet bekend"
                    , UI.Button.pill UI.Style.Potential InitialiseTopscorerResults "Initialiseer"
                    ]

                ( Unauthorised, Fresh (Success results) ) ->
                    [ viewTopscorerResults auth results ]

                ( _, Fresh (Failure err) ) ->
                    [ Element.text (toString err) ]

                ( _, _ ) ->
                    [ Element.text "..." ]
    in
        Element.column UI.Style.None
            [ spacingXY 0 14 ]
            items



-- {teams : List (TeamId, { team: Team, roundsQualified : List (Round, HasQualified)})}


viewTopscorerResults : Access -> TopscorerResults -> Element.Element UI.Style.Style variation Msg
viewTopscorerResults auth results =
    Element.wrappedRow UI.Style.None
        [ spacing 20 ]
        (List.map viewTopscorer results.topscorers)


viewTopscorer : ( HasQualified, Topscorer ) -> Element.Element UI.Style.Style variation Msg
viewTopscorer ( hasQualified, topscorer ) =
    let
        action =
            case hasQualified of
                In ->
                    ChangeTopscorerResults TBD topscorer

                Out ->
                    ChangeTopscorerResults In topscorer

                TBD ->
                    ChangeTopscorerResults Out topscorer
    in
        UI.Button.topscorerBadge hasQualified topscorer action



-- json


encode : TopscorerResults -> Json.Encode.Value
encode results =
    Json.Encode.object
        [ ( "topscorers", Json.Encode.list (List.map encodeTopscorerResult results.topscorers) )
        ]


encodeTopscorerResult : ( HasQualified, Topscorer ) -> Json.Encode.Value
encodeTopscorerResult ( hasQualified, topscorer ) =
    let
        ts =
            encodeTopscorer topscorer

        hq =
            Bets.Types.HasQualified.encode hasQualified
    in
        Json.Encode.object
            [ ( "topscorer", ts )
            , ( "hasQualified", hq )
            ]


encodeTopscorer : Topscorer -> Json.Encode.Value
encodeTopscorer { team, topscorer } =
    Json.Encode.object
        [ ( "team", (Bets.Types.Team.encode team) )
        , ( "name", (Json.Encode.string topscorer) )
        ]


decode : Decoder TopscorerResults
decode =
    Json.Decode.map TopscorerResults
        (field "topscorers" (Json.Decode.list decodeTopscorerResult))


decodeTopscorerResult : Decoder ( HasQualified, Topscorer )
decodeTopscorerResult =
    Json.Decode.map2 (,)
        (field "hasQualified" Bets.Types.HasQualified.decode)
        (field "topscorer" decodeTopscorer)


decodeTopscorer : Decoder Topscorer
decodeTopscorer =
    Json.Decode.map2 Topscorer
        (field "team" Bets.Types.Team.decode)
        (field "name" Json.Decode.string)
