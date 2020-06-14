module Grid exposing
    ( Grid
    , filled
    , get
    , init
    , neighbours
    , set
    , toDict
    , update
    )

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import More.Basics exposing (Int2, Int2Dict)


type Grid a
    = Grid IntSize (Int2Dict a)


init : IntSize -> (Int2 -> a) -> Grid a
init size f =
    IntSize.mapPositions (\p -> ( p, f p )) size
        |> Dict.fromList
        |> Grid size


filled : IntSize -> b -> Grid b
filled size a =
    init size (always a)


toDict : Grid a -> Int2Dict a
toDict (Grid _ d) =
    d


mapDict : (Int2Dict a -> Int2Dict b) -> Grid a -> Grid b
mapDict f (Grid s d) =
    Grid s (f d)


update : Int2 -> (b -> b) -> Grid b -> Grid b
update pos f =
    mapDict (dictUpdateExisting pos f)


dictUpdateExisting : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdateExisting k f =
    Dict.update k (Maybe.map f)


set : Int2 -> b -> Grid b -> Grid b
set pos a =
    update pos (always a)


get : Int2 -> Grid v -> Maybe v
get pos =
    toDict >> Dict.get pos


neighbours : Int2 -> Grid a -> List ( Int2, a )
neighbours pos (Grid size d) =
    let
        dictGetEntry k =
            Dict.get k d |> Maybe.map (Tuple.pair k)
    in
    IntSize.neighbours size pos
        |> List.filterMap dictGetEntry
