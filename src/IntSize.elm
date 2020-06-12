module IntSize exposing
    ( IntSize
    , fromTuple
    , includeNeighbours
    , init
    , isPosInvalid
    , isPosValid
    , neighbourSet
    , neighbours
    , positions
    )

import More.Tuple as Tuple
import Set exposing (Set)


type IntSize
    = IntSize ( Int, Int )


init : Int -> Int -> IntSize
init w h =
    IntSize ( w, h )


fromTuple : ( Int, Int ) -> IntSize
fromTuple =
    IntSize


isPosInvalid : IntSize -> ( Int, Int ) -> Bool
isPosInvalid (IntSize ( w, h )) ( x, y ) =
    x < 0 || y < 0 || x >= w || y >= h


isPosValid : IntSize -> ( Int, Int ) -> Bool
isPosValid s =
    isPosInvalid s >> not


neighbours : IntSize -> ( Int, Int ) -> List ( Int, Int )
neighbours size pos =
    Tuple.neighboursOf pos
        |> List.filter (isPosValid size)


neighbourSet : IntSize -> ( Int, Int ) -> Set ( Int, Int )
neighbourSet size =
    neighbours size >> Set.fromList


includeNeighbours : IntSize -> Set ( Int, Int ) -> Set ( Int, Int )
includeNeighbours size posSet =
    posSet
        |> Set.foldl (neighbourSet size >> Set.union) posSet


positions : IntSize -> List ( Int, Int )
positions (IntSize size) =
    Tuple.range size
