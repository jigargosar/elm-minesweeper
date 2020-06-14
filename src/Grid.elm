module Grid exposing (Grid, filled, init)

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
