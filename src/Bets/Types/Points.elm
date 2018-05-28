module Bets.Types.Points exposing (encode, decode)

import Bets.Json.Encode exposing (mIntEnc)
import Bets.Types exposing (Points)
import Json.Decode exposing (Decoder, maybe)
import Json.Encode


encode : Points -> Json.Encode.Value
encode points =
    mIntEnc points


decode : Decoder Points
decode =
    maybe Json.Decode.int
