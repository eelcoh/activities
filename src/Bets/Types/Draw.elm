module Bets.Types.Draw exposing (team, isComplete, encode, decode)

import Bets.Types exposing (DrawID, Draw, Team)
import Bets.Types.Team as T
import Json.Encode
import Json.Decode exposing (Decoder, maybe, map2, index)


team : Draw -> Maybe Team
team ( _, mTeam ) =
    mTeam


draw : DrawID -> Maybe Team -> Draw
draw drawId mTeam =
    ( drawId, mTeam )


isComplete : Draw -> Bool
isComplete ( _, mTeam ) =
    T.isComplete mTeam



{-
   decode : Decoder Draw
   decode =
       map2 draw
           Json.Decode.string
           (index 0 (maybe T.decode))

-}


decode : Decoder Draw
decode =
    map2 draw
        (Json.Decode.field "draw" Json.Decode.string)
        (Json.Decode.field "team" (maybe T.decode))


encode : Draw -> Json.Encode.Value
encode ( drawId, mTeam ) =
    Json.Encode.object
        [ ( "draw", (Json.Encode.string drawId) )
        , ( "team", (T.encodeMaybe mTeam) )
        ]
