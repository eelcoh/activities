module Bets.Types.HasQualified exposing (encode, decode)

import Json.Encode
import Json.Decode exposing (Decoder, maybe, fail, field, lazy)
import Bets.Types exposing (HasQualified(..))


encode : HasQualified -> Json.Encode.Value
encode hasQ =
    Json.Encode.string (toString hasQ)


decode : Decoder HasQualified
decode =
    Json.Decode.string
        |> Json.Decode.map toHasQualified


toStringHasQualified : HasQualified -> String
toStringHasQualified hasQ =
    case hasQ of
        TBD ->
            "TBD"

        In ->
            "In"

        Out ->
            "Out"


toHasQualified : String -> HasQualified
toHasQualified hasQStr =
    case hasQStr of
        "TBD" ->
            TBD

        "In" ->
            In

        "Out" ->
            Out

        _ ->
            TBD
