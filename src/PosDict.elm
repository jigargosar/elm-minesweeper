module PosDict exposing (PosDict, filled, init)

import Dict exposing (Dict)
import IntSize exposing (IntSize)


type alias PosDict a =
    Dict ( Int, Int ) a


init : IntSize -> (( Int, Int ) -> a) -> PosDict a
init size f =
    List.foldl (\pos -> Dict.insert pos (f pos)) Dict.empty (IntSize.positions size)


filled : IntSize -> b -> PosDict b
filled size a =
    init size (always a)
