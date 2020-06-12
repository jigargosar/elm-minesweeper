module Grid exposing (Grid, get, init, toDict)

import Dict exposing (Dict)
import More.Tuple as Tuple


type Grid a
    = Grid (Dict ( Int, Int ) a)


init : ( Int, Int ) -> (( Int, Int ) -> a) -> Grid a
init size f =
    List.foldl (\pos -> Dict.insert pos (f pos)) Dict.empty (Tuple.range size)
        |> Grid


toDict : Grid a -> Dict ( Int, Int ) a
toDict (Grid d) =
    d


get : ( Int, Int ) -> Grid a -> Maybe a
get k (Grid d) =
    Dict.get k d
