module Bets.Types.Score exposing (homeScore, awayScore, asString, merge, isComplete, decode, encode, encodeMaybe)

import Json.Encode
import Json.Decode exposing (Decoder, maybe, index, map2)
import Bets.Types exposing (Score)
import Bets.Json.Encode exposing (mIntEnc)


homeScore : Score -> Maybe Int
homeScore ( mInt, _ ) =
    mInt


awayScore : Score -> Maybe Int
awayScore ( _, mInt ) =
    mInt



-- Json handling


score : Maybe Int -> Maybe Int -> Score
score h a =
    ( h, a )


merge : Score -> Score -> Score
merge newScore oldScore =
    let
        ( newHome, newAway ) =
            newScore

        ( oldHome, oldAway ) =
            oldScore

        home =
            case newHome of
                Just _ ->
                    newHome

                Nothing ->
                    oldHome

        away =
            case newAway of
                Just _ ->
                    newAway

                Nothing ->
                    oldHome

        score =
            ( home, away )
    in
        score


asString : Score -> String
asString ( mH, mA ) =
    let
        str mInt =
            Maybe.map toString mInt |> Maybe.withDefault "_"
    in
        List.foldr (++) "" [ (str mH), "-", (str mA) ]


isComplete : Maybe Score -> Bool
isComplete mScore =
    case mScore of
        Just ( Just _, Just _ ) ->
            True

        _ ->
            False


decode : Decoder Score
decode =
    map2 score
        (index 0 (maybe Json.Decode.int))
        (index 1 (maybe Json.Decode.int))


encodeMaybe : Maybe Score -> Json.Encode.Value
encodeMaybe mScore =
    case mScore of
        Nothing ->
            Json.Encode.null

        Just s ->
            encode s


encode : Score -> Json.Encode.Value
encode ( mHome, mAway ) =
    Json.Encode.list
        [ (mIntEnc mHome)
        , (mIntEnc mAway)
        ]
