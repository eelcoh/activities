module Bets.Types.Bracket
    exposing
        ( set
        , setBulk
        , proceed
        , proceedHome
        , proceedAway
        , winner
        , unsetQualifier
        , get
        , qualifier
        , display
        , isComplete
        , encode
        , decode
        , decodeHasQualified
        , decodeWinner
        )

import Json.Encode
import Json.Decode exposing (Decoder, maybe, fail, field, lazy)
import Bets.Types exposing (Team, Bracket(..), Qualifier, Winner(..), Slot, HasQualified(..), Group)
import Bets.Types.Team as T
import Bets.Types.Round as R
import Maybe.Extra as M


reset : Bracket -> Qualifier -> Bracket
reset newBracket prevWinner =
    case newBracket of
        MatchNode slot w home away rnd hasQ ->
            let
                currentWinner =
                    winner newBracket

                newWinner =
                    if currentWinner == prevWinner then
                        w
                    else
                        None
            in
                -- should really reassess hasQ, but not necessary now since it can only be TBD
                MatchNode slot newWinner home away rnd hasQ

        TeamNode slot qualifier hasQ ->
            TeamNode slot qualifier hasQ


proceedHome : Bracket -> Slot -> Bracket
proceedHome bracket slot =
    proceed bracket slot HomeTeam


proceedAway : Bracket -> Slot -> Bracket
proceedAway bracket slot =
    proceed bracket slot AwayTeam


proceed : Bracket -> Slot -> Winner -> Bracket
proceed bracket slot wnnr =
    case bracket of
        MatchNode s w home away rnd hasQ ->
            if s == slot then
                -- should really reassess hasQ, but not necessary now since it can only be TBD
                MatchNode slot wnnr home away rnd hasQ
            else
                let
                    newLeft =
                        proceed home slot wnnr

                    newRight =
                        proceed away slot wnnr

                    currentWinner =
                        winner bracket

                    -- should really reassess hasQ, but not necessary now since it can only be TBD
                    newBracket =
                        MatchNode s w newLeft newRight rnd hasQ
                in
                    reset newBracket currentWinner

        _ ->
            bracket


winner : Bracket -> Qualifier
winner bracket =
    case bracket of
        MatchNode slot w home away _ _ ->
            case w of
                HomeTeam ->
                    winner home

                AwayTeam ->
                    winner away

                None ->
                    Nothing

        TeamNode slot qualifier hasQ ->
            qualifier


set : Bracket -> Slot -> Qualifier -> Bracket
set bracket slot qualifier =
    case bracket of
        MatchNode s w home away round hasQ ->
            let
                newHome =
                    set home slot qualifier

                newAway =
                    set away slot qualifier

                newBracket =
                    MatchNode s w newHome newAway round hasQ

                currentWinner =
                    winner bracket
            in
                reset newBracket currentWinner

        TeamNode s mt hasQ ->
            if s == slot then
                TeamNode slot qualifier hasQ
            else
                bracket


setBulk : Bracket -> List ( Slot, Qualifier ) -> Bracket
setBulk bracket slots =
    let
        newSet ( slot, qualifier ) brkt =
            set brkt slot qualifier
    in
        List.foldl newSet bracket slots


unsetQualifier : Bracket -> Qualifier -> Bracket
unsetQualifier bracket qualifier =
    case bracket of
        MatchNode s w home away round hasQ ->
            let
                newHome =
                    unsetQualifier home qualifier

                newAway =
                    unsetQualifier away qualifier

                newBracket =
                    MatchNode s w newHome newAway round hasQ

                currentWinner =
                    winner bracket
            in
                reset newBracket currentWinner

        TeamNode slot mt hasQ ->
            if mt == qualifier then
                TeamNode slot Nothing hasQ
            else
                bracket


qualifier : Bracket -> Qualifier
qualifier bracket =
    case bracket of
        MatchNode s w home away round hasQ ->
            case w of
                HomeTeam ->
                    qualifier home

                AwayTeam ->
                    qualifier away

                None ->
                    Nothing

        TeamNode s q _ ->
            q


get : Bracket -> Slot -> Maybe Bracket
get brkt slot =
    case brkt of
        MatchNode s w home away round hasQ ->
            if s == slot then
                Just brkt
            else
                let
                    oneOf =
                        M.values >> List.head
                in
                    oneOf [ (get away slot), (get home slot) ]

        TeamNode s q hasQ ->
            if s == slot then
                Just brkt
            else
                Nothing


display : Bracket -> String
display bracket =
    T.mdisplay (qualifier bracket)


isComplete : Bracket -> Bool
isComplete brkt =
    case brkt of
        TeamNode slot qualifier hasQ ->
            M.isJust qualifier

        MatchNode slot w home away round hasQ ->
            case w of
                None ->
                    False

                _ ->
                    isComplete home && isComplete away


encodeWinner : Winner -> Json.Encode.Value
encodeWinner winner =
    case winner of
        HomeTeam ->
            Json.Encode.string "HomeTeam"

        AwayTeam ->
            Json.Encode.string "AwayTeam"

        None ->
            Json.Encode.null


encode : Bracket -> Json.Encode.Value
encode bracket =
    case bracket of
        TeamNode slot qualifier hasQ ->
            Json.Encode.object
                [ ( "node", Json.Encode.string "team" )
                , ( "slot", Json.Encode.string slot )
                , ( "qualifier", T.encodeMaybe qualifier )
                , ( "hasQualified", encodeHasQualified hasQ )
                ]

        MatchNode slot winner home away round hasQ ->
            Json.Encode.object
                [ ( "node", Json.Encode.string "match" )
                , ( "slot", Json.Encode.string slot )
                , ( "winner", encodeWinner winner )
                , ( "home", encode home )
                , ( "away", encode away )
                , ( "round", R.encode round )
                , ( "hasQualified", encodeHasQualified hasQ )
                ]


decode : Decoder Bracket
decode =
    (field "node" Json.Decode.string)
        |> Json.Decode.andThen
            (\node ->
                case node of
                    "team" ->
                        Json.Decode.map3 TeamNode
                            (field "slot" Json.Decode.string)
                            (field "qualifier" (maybe T.decode))
                            (field "hasQualified" decodeHasQualified)

                    "match" ->
                        Json.Decode.map6 MatchNode
                            (field "slot" Json.Decode.string)
                            (field "winner" ((maybe Json.Decode.string) |> Json.Decode.andThen decodeWinner))
                            (field "home" (lazy (\_ -> decode)))
                            (field "away" (lazy (\_ -> decode)))
                            (field "round" R.decode)
                            (field "hasQualified" decodeHasQualified)

                    _ ->
                        fail (node ++ " is not a recognized node for brackets")
            )


decodeWinner : Maybe String -> Decoder Winner
decodeWinner w =
    let
        stringToWinner winner =
            case winner of
                "HomeTeam" ->
                    HomeTeam

                "AwayTeam" ->
                    AwayTeam

                _ ->
                    None
    in
        case w of
            Nothing ->
                Json.Decode.succeed None

            Just wnr ->
                Json.Decode.succeed (stringToWinner wnr)


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


encodeHasQualified : HasQualified -> Json.Encode.Value
encodeHasQualified hasQ =
    Json.Encode.string (toString hasQ)


decodeHasQualified : Decoder HasQualified
decodeHasQualified =
    Json.Decode.string
        |> Json.Decode.map toHasQualified
