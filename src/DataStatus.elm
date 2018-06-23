module DataStatus exposing (map, update)

import Types exposing (DataStatus(..))


map : (a -> b) -> DataStatus a -> DataStatus b
map f status =
    case status of
        Fresh a ->
            f a
                |> Dirty

        Stale a ->
            f a
                |> Stale

        Dirty a ->
            f a
                |> Dirty


update : a -> DataStatus a -> DataStatus a
update data status =
    Fresh data
