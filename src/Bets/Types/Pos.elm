module Bets.Types.Pos exposing (encode, decode)

import Bets.Types exposing (Pos(..))
import Json.Encode
import Json.Decode exposing (Decoder, succeed, andThen)


toString : Pos -> String
toString pos =
    case pos of
        First ->
            "First"

        Second ->
            "Second"

        Third ->
            "Third"

        TopThird ->
            "TopThird"

        Free ->
            "Free"


toPos : String -> Pos
toPos s =
    case s of
        "First" ->
            First

        "Second" ->
            Second

        "Third" ->
            Third

        "TopThird" ->
            TopThird

        _ ->
            Free


encode : Pos -> Json.Encode.Value
encode pos =
    toString pos
        |> Json.Encode.string


decode : Decoder Pos
decode =
    Json.Decode.string
        |> Json.Decode.map toPos
