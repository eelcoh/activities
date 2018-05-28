module Bets.Types.Stadium exposing (decode, encode)

import Json.Encode
import Json.Decode exposing (Decoder, maybe, field)
import Bets.Types exposing (Stadium)


decode : Decoder Stadium
decode =
    Json.Decode.map2 Stadium
        (field "stadium" Json.Decode.string)
        (field "town" Json.Decode.string)


encode : Stadium -> Json.Encode.Value
encode { stadium, town } =
    Json.Encode.object
        [ ( "stadium", Json.Encode.string stadium )
        , ( "town", Json.Encode.string town )
        ]
