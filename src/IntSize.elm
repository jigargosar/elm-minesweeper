module IntSize exposing
    ( IntSize
    , includeNeighbours
    , init
    , neighbourSet
    , neighbours
    , positions
    )

import More.Basics exposing (..)
import More.Tuple as Tuple
import Set exposing (Set)


type IntSize
    = Size Int2


init : Int -> Int -> IntSize
init w h =
    Size ( w, h )


isPosInvalid : IntSize -> ( Int, Int ) -> Bool
isPosInvalid (Size ( w, h )) ( x, y ) =
    x < 0 || y < 0 || x >= w || y >= h


isPosValid : IntSize -> ( Int, Int ) -> Bool
isPosValid s =
    isPosInvalid s >> not


foldPositions : (Int2 -> a) -> IntSize -> List a
foldPositions f (Size ( w, h )) =
    List.range 0 (w - 1)
        |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (\y -> f ( x, y )))


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
positions (Size size) =
    Tuple.range size
