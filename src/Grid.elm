module Grid exposing (Grid, filled, init)

import Dict exposing (Dict)
import GridSize exposing (GridSize)
import Set


type alias I2 =
    ( Int, Int )


type alias I2Dict a =
    Dict I2 a


type Grid a
    = Grid GridSize (I2Dict a)


filled : GridSize -> a -> Grid a
filled size a =
    Set.foldl (\pos -> Dict.insert pos a) Dict.empty (GridSize.posSet size)
        |> Grid size


init : GridSize -> (( Int, Int ) -> a) -> Grid a
init size f =
    Set.foldl (\pos -> Dict.insert pos (f pos)) Dict.empty (GridSize.posSet size)
        |> Grid size
