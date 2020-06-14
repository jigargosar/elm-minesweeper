module Grid exposing
    ( Grid
    , filled
    , filterNeighbours
    , get
    , includeNeighbours
    , init
    , neighbourPositions
    , neighbourSet
    , neighbours
    , set
    , toDict
    , update
    )

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import PosDict exposing (PosDict)
import Set exposing (Set)


type Grid a
    = Grid IntSize (PosDict a)


init : IntSize -> (( Int, Int ) -> a) -> Grid a
init size f =
    PosDict.init size f
        |> Grid size


filled : IntSize -> b -> Grid b
filled size a =
    init size (always a)


toDict : Grid a -> PosDict a
toDict (Grid _ d) =
    d


mapDict : (PosDict a -> PosDict b) -> Grid a -> Grid b
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


neighbourSet : ( Int, Int ) -> Grid a -> Set ( Int, Int )
neighbourSet pos (Grid size _) =
    IntSize.neighbourSet size pos


neighbourPositions : ( Int, Int ) -> Grid a -> List ( Int, Int )
neighbourPositions pos (Grid size _) =
    IntSize.neighbours size pos


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


filterNeighbours : ( Int, Int ) -> (a -> Bool) -> Grid a -> Set ( Int, Int )
filterNeighbours pos isOk grid =
    let
        func nPos =
            case get nPos grid of
                Nothing ->
                    False

                Just a ->
                    isOk a
    in
    Set.filter func (neighbourSet pos grid)
