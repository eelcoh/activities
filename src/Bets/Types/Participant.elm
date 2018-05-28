module Bets.Types.Participant
    exposing
        ( setName
        , setAddress
        , setPhoneNumber
        , setEmail
        , init
        , isComplete
        , encode
        , decode
        )

import Json.Encode
import Json.Decode exposing (Decoder, maybe, field)
import Maybe.Extra as M
import Bets.Json.Encode exposing (mStrEnc)
import Bets.Types exposing (Participant)


init : Participant
init =
    Participant Nothing Nothing Nothing Nothing Nothing Nothing


setName : Participant -> String -> Participant
setName participant name =
    { participant | name = Just name }


setAddress : Participant -> String -> Participant
setAddress participant address =
    { participant | address = Just address }


setPhoneNumber : Participant -> String -> Participant
setPhoneNumber participant phone =
    { participant | phone = Just phone }


setEmail : Participant -> String -> Participant
setEmail participant email =
    { participant | email = Just email }


setHowyouknowus : Participant -> String -> Participant
setHowyouknowus participant howyouknowus =
    { participant | howyouknowus = Just howyouknowus }


isComplete : Participant -> Bool
isComplete participant =
    [ participant.name, participant.address, participant.phone, participant.email, participant.howyouknowus ]
        |> M.combine
        |> M.isJust


decode : Decoder Participant
decode =
    Json.Decode.map6 Participant
        (maybe (field "name" Json.Decode.string))
        (maybe (field "address" Json.Decode.string))
        (maybe (field "residence" Json.Decode.string))
        (maybe (field "phone" Json.Decode.string))
        (maybe (field "email" Json.Decode.string))
        (maybe (field "howyouknowus" Json.Decode.string))


encode : Participant -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ( "name", mStrEnc p.name )
        , ( "address", mStrEnc p.address )
        , ( "residence", mStrEnc p.residence )
        , ( "phone", mStrEnc p.phone )
        , ( "email", mStrEnc p.email )
        , ( "howyouknowus", mStrEnc p.howyouknowus )
        ]
