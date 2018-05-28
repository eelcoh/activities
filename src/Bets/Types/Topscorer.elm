module Bets.Types.Topscorer
    exposing
        ( getTeam
        , setTeam
        , getPlayer
        , setPlayer
        , isComplete
        , decode
        , encode
        )

import Json.Encode
import Json.Decode exposing (Decoder, maybe, map2, field)
import Bets.Json.Encode exposing (mStrEnc)
import Bets.Types exposing (Topscorer, Team, Player)
import Bets.Types.Team
import Maybe.Extra as M
import Tuple exposing (first, second)


getTeam : Topscorer -> Maybe Team
getTeam ts =
    second ts



{-
   Sets or toggles the team. As team changes, topscorer player is always cleared.
-}


setTeam : Topscorer -> Team -> Topscorer
setTeam ts team =
    case (getTeam ts) of
        Nothing ->
            ( Nothing, Just team )

        Just t ->
            if (t == team) then
                ( Nothing, Nothing )
            else
                ( Nothing, Just team )


getPlayer : Topscorer -> Maybe Player
getPlayer ts =
    first ts


setPlayer : Topscorer -> Player -> Topscorer
setPlayer ( mPlayer, mTeam ) player =
    case mPlayer of
        Nothing ->
            ( Just player, mTeam )

        -- we should actually check whether that is possible. Todo.
        Just p ->
            if (p == player) then
                ( Nothing, mTeam )
            else
                ( Just player, mTeam )


topscorer : Maybe String -> Maybe Team -> Topscorer
topscorer mName mTeam =
    ( mName, mTeam )


isComplete : Topscorer -> Bool
isComplete ( mName, mTeam ) =
    (M.isJust mName) && (M.isJust mTeam)


encode : Topscorer -> Json.Encode.Value
encode ( mName, mTeam ) =
    Json.Encode.object
        [ ( "name", mStrEnc mName )
        , ( "team", Bets.Types.Team.encodeMaybe mTeam )
        ]


decode : Decoder Topscorer
decode =
    Json.Decode.map
        (\x -> topscorer x.name x.team)
        decodeTSObj


type alias TopscorerObject =
    { name : Maybe String
    , team : Maybe Team
    }


decodeTSObj : Decoder TopscorerObject
decodeTSObj =
    Json.Decode.map2
        TopscorerObject
        (field "name" (maybe Json.Decode.string))
        (field "team" (maybe Bets.Types.Team.decode))
