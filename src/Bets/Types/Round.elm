module Bets.Types.Round
    exposing
        ( toInt
        , stringToRound
        , nextRound
        , isSameOrANextRound
        , compareRounds
        , encode
        , decode
        )

import Bets.Types exposing (Round(..))
import Json.Encode
import Json.Decode exposing (Decoder, andThen, succeed)


-- MODEL


stringToRound : String -> Maybe Round
stringToRound r =
    case r of
        "I" ->
            Just I

        "II" ->
            Just II

        "III" ->
            Just III

        "IV" ->
            Just IV

        "V" ->
            Just V

        "VI" ->
            Just VI

        _ ->
            Nothing


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


compareRounds : Round -> Round -> Order
compareRounds r1 r2 =
    compare (toInt r1) (toInt r2)


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
