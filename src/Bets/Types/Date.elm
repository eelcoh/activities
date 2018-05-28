module Bets.Types.Date exposing (toDate, encode, decode)

import Bets.Types exposing (DateString)
import Json.Encode
import Json.Decode exposing (Decoder)
import Date
import Result


encode : Date.Date -> Json.Encode.Value
encode date =
    Json.Encode.float (Date.toTime date)


decode : Decoder Date.Date
decode =
    Json.Decode.float
        |> Json.Decode.map Date.fromTime


toDate : DateString -> Date.Date
toDate dateStr =
    Result.withDefault (Date.fromTime 0) (Date.fromString dateStr)
