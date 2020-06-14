module Grid exposing
    ( Grid
    , filled
    , get
    , includeNeighbours
    , init
    , neighbours
    , set
    , toDict
    , update
    )

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import More.Basics exposing (Int2Dict)
import Set exposing (Set)


type Grid a
    = Grid IntSize (Int2Dict a)


init : IntSize -> (( Int, Int ) -> a) -> Grid a
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


update : ( Int, Int ) -> (b -> b) -> Grid b -> Grid b
update pos f =
    mapDict (dictUpdateExisting pos f)


dictUpdateExisting : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdateExisting k f =
    Dict.update k (Maybe.map f)


set : ( Int, Int ) -> b -> Grid b -> Grid b
set pos a =
    update pos (always a)


get : ( Int, Int ) -> Grid v -> Maybe v
get pos =
    toDict >> Dict.get pos


neighbours : ( Int, Int ) -> Grid a -> List ( ( Int, Int ), a )
neighbours pos (Grid size d) =
    let
        dictGetEntry k =
            Dict.get k d |> Maybe.map (Tuple.pair k)
    in
    IntSize.neighbours size pos
        |> List.filterMap dictGetEntry


includeNeighbours : Grid a -> Set ( Int, Int ) -> Set ( Int, Int )
includeNeighbours (Grid size _) pos =
    IntSize.includeNeighbours size pos
