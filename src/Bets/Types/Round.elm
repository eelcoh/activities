module Bets.Types.Round
    exposing
        ( toInt
        , nextRound
        , isSameOrANextRound
        , encode
        , decode
        )

import Bets.Types exposing (Round(..))
import Json.Encode
import Json.Decode exposing (Decoder, andThen, succeed)


-- MODEL


toInt : Round -> Int
toInt r =
    case r of
        I ->
            1

        II ->
            2

        III ->
            3

        IV ->
            4

        V ->
            5

        VI ->
            6


toRound : Int -> Round
toRound i =
    case i of
        1 ->
            I

        2 ->
            II

        3 ->
            III

        4 ->
            IV

        5 ->
            V

        _ ->
            VI


isSameOrANextRound : Round -> Round -> Bool
isSameOrANextRound r1 r2 =
    (toInt r1) >= (toInt r2)


nextRound : Round -> Maybe Round
nextRound r =
    case r of
        I ->
            Just II

        II ->
            Just III

        III ->
            Just IV

        IV ->
            Just V

        V ->
            Just VI

        VI ->
            Nothing


encode : Round -> Json.Encode.Value
encode r =
    Json.Encode.int (toInt r)



{-
   decodeRound : Int -> Decoder Round
   decodeRound i =
     succeed (toRound i)

   decode : Decoder Round
   decode =
     "round" := Json.Decode.int `andThen` decodeRound
-}


decode : Decoder Round
decode =
    Json.Decode.int
        |> Json.Decode.map toRound
