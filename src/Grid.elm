module Grid exposing (Grid, filled, get, init, set, toDict, update)

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import PosDict exposing (PosDict)


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


set : ( Int, Int ) -> b -> Grid b -> Grid b
set pos a =
    update pos (always a)


get : ( Int, Int ) -> Grid v -> Maybe v
get pos =
    toDict >> Dict.get pos


dictUpdateExisting : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdateExisting k f =
    Dict.update k (Maybe.map f)


dictGet2 : comparable -> Dict comparable v -> Dict comparable a -> Maybe ( v, a )
dictGet2 k a b =
    Maybe.map2 Tuple.pair (Dict.get k a) (Dict.get k b)


dictSetExisting : comparable -> b -> Dict comparable b -> Dict comparable b
dictSetExisting k v =
    dictUpdateExisting k (always v)
