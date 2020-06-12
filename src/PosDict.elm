module PosDict exposing (..)

import Dict exposing (Dict)
import IntSize exposing (IntSize)


type alias PosDict a =
    Dict ( Int, Int ) a


init : IntSize -> (( Int, Int ) -> a) -> PosDict a
init size f =
    List.foldl (\pos -> Dict.insert pos (f pos)) Dict.empty (IntSize.positions size)
