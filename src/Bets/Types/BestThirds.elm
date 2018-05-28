module Bets.Types.BestThirds
    exposing
        ( updateChoices
        , teamsToReset
        , isComplete
        , encode
        , decode
        )

import Json.Encode
import Json.Decode exposing (Decoder, map3, index)
import Bets.Types exposing (Group(..), Team, BestThird, BestThirds, DrawID)
import Bets.Types.Team exposing (..)
import Bets.Types.Group
import String
import Set


type alias Choice =
    ( Group, Team )


type alias Choices =
    List Choice


type alias Assignment =
    ( Choice, DrawID )



-- Exposed functions ----


updateChoices : Group -> Team -> BestThirds -> BestThirds
updateChoices group team currentBestThirds =
    let
        choice =
            ( group, team )

        currentChoices =
            extractChoices currentBestThirds

        bToggleOff =
            List.any (isChoice choice) currentChoices

        bGroupToggle =
            List.any (isGroup choice) currentChoices

        bFreeSlot =
            List.length currentChoices < 4

        tempChoices =
            newChoices bToggleOff bGroupToggle bFreeSlot choice currentChoices
    in
        reorderChoices tempChoices
            |> toBestThirds


teamsToReset : BestThirds -> BestThirds -> List Team
teamsToReset oldAssignments newAssignments =
    let
        thrd =
            (\( _, _, d ) -> d)

        sortedOld =
            List.sortBy thrd oldAssignments

        sortedNew =
            List.sortBy thrd newAssignments
    in
        case ( sortedOld, sortedNew ) of
            -- base assumption:
            -- the triplets are sorted on the third element: the drawId
            -- the two are then processed 'balanced line'
            -- if both lists are empty, the result is empty
            ( [], [] ) ->
                []

            -- if the first list is empty and the second isn't, the team in the second
            --
            ( [], ( _, t2, _ ) :: restNew ) ->
                t2 :: teamsToReset [] restNew

            ( ( _, t1, _ ) :: restOld, [] ) ->
                t1 :: teamsToReset [] restOld

            ( ( g1, t1, d1 ) :: restOld, ( g2, t2, d2 ) :: restNew ) ->
                case (compare d1 d2) of
                    GT ->
                        t2 :: teamsToReset sortedOld restNew

                    LT ->
                        t1 :: teamsToReset restOld sortedNew

                    EQ ->
                        if (t1 == t2) then
                            teamsToReset restOld restNew
                        else
                            t1 :: t2 :: teamsToReset restOld restNew



-- RECREATION ---


toBestThirds : List ( Choice, DrawID ) -> BestThirds
toBestThirds assignedChoices =
    let
        toBestThird ( ( g, t ), dId ) =
            ( g, t, dId )
    in
        List.map toBestThird assignedChoices



-- CHECKS (not exposed) ----


isChoice : Choice -> Choice -> Bool
isChoice ( gNew, tNew ) ( gOld, tOld ) =
    (gNew == gOld) && (tNew == tOld)


isGroup : Choice -> Choice -> Bool
isGroup ( gNew, _ ) ( gOld, _ ) =
    (gNew == gOld)



-- UPDATES (not exposed) ----
{-
   make new choices:
   - if the current choice was already in the list, it should be removed from the list (toggle off)
   - if the current choice was not in the list, but another team from the same group was, that team should be replaced
   - if none of the above, and there is a free slot, the new choice will be added
   - else, the list is full, nothing changes
-}


newChoices : Bool -> Bool -> Bool -> Choice -> Choices -> Choices
newChoices mToggleOff mGroupToggle mFreeSlot choice currentChoices =
    case ( mToggleOff, mGroupToggle, mFreeSlot ) of
        ( True, _, _ ) ->
            resetChoice choice currentChoices

        ( False, True, _ ) ->
            updateChoice choice currentChoices

        ( False, False, True ) ->
            choice :: currentChoices

        _ ->
            currentChoices



{- replace a choice for a BestThird from the same group -}


updateChoice : Choice -> Choices -> Choices
updateChoice choice choices =
    case choices of
        [] ->
            []

        c :: choices ->
            if isGroup choice c then
                choice :: choices
            else
                c :: updateChoice choice choices



{- the selected choice was already in the list, implies toggling off this choice -}


resetChoice : Choice -> Choices -> Choices
resetChoice choice choices =
    case choices of
        [] ->
            []

        c :: choices ->
            if isChoice choice c then
                choices
            else
                c :: resetChoice choice choices



-- Reordering ----


{-|
Once the four best thirds have been determined, these need to be placed on the bracket. This is using the following scheme:

The four best-placed teams are:
1 2 3 4  | WA WB WC WD play
---------|-----------------
A B C D  | 3C 3D 3A 3B
A B C E  | 3C 3A 3B 3E
A B C F  | 3C 3A 3B 3F
A B D E  | 3D 3A 3B 3E
A B D F  | 3D 3A 3B 3F
A B E F  | 3E 3A 3B 3F
A C D E  | 3C 3D 3A 3E
A C D F  | 3C 3D 3A 3F
A C E F  | 3C 3A 3F 3E
A D E F  | 3D 3A 3F 3E
B C D E  | 3C 3D 3B 3E
B C D F  | 3C 3D 3B 3F
B C E F  | 3E 3C 3B 3F
B D E F  | 3E 3D 3B 3F
C D E F  | 3C 3D 3F 3E
-}
order : String -> List ( DrawID, Group )
order groups =
    case groups of
        "ABCD" ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", A ), ( "T4", B ) ]

        "ABCE" ->
            [ ( "T1", C ), ( "T2", A ), ( "T3", B ), ( "T4", E ) ]

        "ABCF" ->
            [ ( "T1", C ), ( "T2", A ), ( "T3", B ), ( "T4", F ) ]

        "ABDE" ->
            [ ( "T1", D ), ( "T2", A ), ( "T3", B ), ( "T4", E ) ]

        "ABDF" ->
            [ ( "T1", D ), ( "T2", A ), ( "T3", B ), ( "T4", F ) ]

        "ABEF" ->
            [ ( "T1", E ), ( "T2", A ), ( "T3", B ), ( "T4", F ) ]

        "ACDE" ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", A ), ( "T4", E ) ]

        "ACDF" ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", A ), ( "T4", F ) ]

        "ACEF" ->
            [ ( "T1", C ), ( "T2", A ), ( "T3", F ), ( "T4", E ) ]

        "ADEF" ->
            [ ( "T1", D ), ( "T2", A ), ( "T3", F ), ( "T4", E ) ]

        "BCDE" ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", B ), ( "T4", E ) ]

        "BCDF" ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", B ), ( "T4", F ) ]

        "BCEF" ->
            [ ( "T1", E ), ( "T2", C ), ( "T3", B ), ( "T4", F ) ]

        "BDEF" ->
            [ ( "T1", E ), ( "T2", D ), ( "T3", B ), ( "T4", F ) ]

        "CDEF" ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", F ), ( "T4", E ) ]

        _ ->
            [ ( "T1", C ), ( "T2", D ), ( "T3", A ), ( "T4", B ) ]


reorderChoices : List Choice -> List ( Choice, DrawID )
reorderChoices choices =
    let
        tupleCompare : ( DrawID, Group ) -> ( DrawID, Group ) -> Order
        tupleCompare ( _, g1 ) ( _, g2 ) =
            gcompare g1 g2

        choiceCompare : Choice -> Choice -> Order
        choiceCompare ( g1, _ ) ( g2, _ ) =
            gcompare g1 g2

        sChoices =
            List.sortWith choiceCompare choices

        groupsString : String
        groupsString =
            List.map Tuple.first sChoices
                |> List.map toString
                |> String.concat

        -- e.g ABDF
    in
        -- start with []
        order groupsString
            -- e.g. [("T1", E), ("T2", A), ("T3", B), ("T4", F)]
            --|> List.map (\(a, b) -> (b, a))
            |>
                List.sortWith tupleCompare
            -- e.g. [("T2", A), ("T3", B), ("T1", E), ("T4", F)]
            |>
                List.map Tuple.first
            -- e.g. ["T2", "T3" , "T1", "T4"]
            |>
                List.map2 (,) sChoices



-- e.g. [((A, FRA), "T2"), ((B, x), "T3"), ((E, y), "T1"), ((F, z), "T4")]
-- Utilities


gcompare : Group -> Group -> Order
gcompare g1 g2 =
    compare (toString g1) (toString g2)


extractChoices : BestThirds -> List Choice
extractChoices bts =
    List.map (\( g, t, d ) -> ( g, t )) bts


toBestThird : Group -> Team -> DrawID -> BestThird
toBestThird =
    (,,)


isComplete : BestThirds -> Bool
isComplete bts =
    let
        numberOfBts =
            List.length bts

        numberOfGroups =
            List.map (\( g, t, d ) -> Bets.Types.Group.toString g) bts
                |> Set.fromList
                |> Set.size

        numberOfTeams =
            List.map (\( g, t, d ) -> t.teamID) bts
                |> Set.fromList
                |> Set.size
    in
        (numberOfBts == 4)
            && (numberOfTeams == 4)
            && (numberOfGroups == 4)


encodeBestThird : ( Group, Team, DrawID ) -> Json.Encode.Value
encodeBestThird ( group, team, drawID ) =
    Json.Encode.list
        [ Bets.Types.Group.encode group
        , Bets.Types.Team.encode team
        , Json.Encode.string drawID
        ]


encode : BestThirds -> Json.Encode.Value
encode bestThirds =
    List.map encodeBestThird bestThirds
        |> Json.Encode.list


decodeBestThird : Decoder BestThird
decodeBestThird =
    Json.Decode.map3 toBestThird
        (index 0 Bets.Types.Group.decode)
        (index 1 Bets.Types.Team.decode)
        (index 2 Json.Decode.string)


decode : Decoder BestThirds
decode =
    Json.Decode.list decodeBestThird
