module Grid exposing
    ( Grid
    , filled
    , get
    , includeNeighboursPosSet
    , init
    , neighbourPosSet
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


neighbourPosSet : Grid a -> ( Int, Int ) -> Set ( Int, Int )
neighbourPosSet (Grid size _) pos =
    IntSize.neighbourSet size pos


includeNeighboursPosSet : Grid a -> Set ( Int, Int ) -> Set ( Int, Int )
includeNeighboursPosSet (Grid size _) pos =
    IntSize.includeNeighbours size pos


filterNeighbourPosSet : ( Int, Int ) -> (( Int, Int ) -> a -> Bool) -> Grid a -> Set ( Int, Int )
filterNeighbourPosSet pos isOk grid =
    neighbourPosSet grid pos
        |> Set.filter
            (\nPos ->
                case get nPos grid of
                    Nothing ->
                        False

                    Just a ->
                        isOk pos a
            )
