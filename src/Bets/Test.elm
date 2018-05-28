module Bets.Test exposing (clean)


cleanInt : Int -> Maybe Int -> Maybe Int
cleanInt i mInt =
    case mInt of
        Nothing ->
            Nothing

        Just n ->
            if i == n then
                Nothing
            else
                Just n


cleanInts : List (Maybe Int) -> Int -> List (Maybe Int)
cleanInts l i =
    List.map (cleanInt i) l


clean : List (Maybe Int) -> List Int -> List (Maybe Int)
clean l1 f =
    case f of
        h :: rest ->
            let
                l2 =
                    cleanInts l1 h
            in
                clean l2 rest

        [] ->
            l1
