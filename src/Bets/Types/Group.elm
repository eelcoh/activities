module Bets.Types.Group exposing (encode, decode, toString, toGroup)

import Json.Encode
import Json.Decode exposing (Decoder)
import Bets.Types exposing (Group(..))


toString : Group -> String
toString grp =
    case grp of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        H ->
            "H"


toGroup : String -> Group
toGroup s =
    case s of
        "A" ->
            A

        "B" ->
            B

        "C" ->
            C

        "D" ->
            D

        "E" ->
            E

        "F" ->
            F

        "G" ->
            G

        _ ->
            H


encode : Group -> Json.Encode.Value
encode grp =
    Json.Encode.string (toString grp)


decode : Decoder Group
decode =
    Json.Decode.string
        |> Json.Decode.map toGroup
