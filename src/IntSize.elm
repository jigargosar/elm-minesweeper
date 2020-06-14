module IntSize exposing
    ( IntSize
    , includeNeighbours
    , init
    , mapPositions
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


mapPositions : (Int2 -> a) -> IntSize -> List a
mapPositions f (Size ( w, h )) =
    List.range 0 (w - 1)
        |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (\y -> f ( x, y )))


positions : IntSize -> List Int2
positions =
    mapPositions identity


neighbours : IntSize -> Int2 -> List Int2
neighbours size pos =
    let
        isPosInvalid : IntSize -> Int2 -> Bool
        isPosInvalid (Size ( w, h )) ( x, y ) =
            x < 0 || y < 0 || x >= w || y >= h

        isPosValid : IntSize -> Int2 -> Bool
        isPosValid s =
            isPosInvalid s >> not
    in
    Tuple.neighboursOf pos
        |> List.filter (isPosValid size)


includeNeighbours : IntSize -> Set Int2 -> Set Int2
includeNeighbours size posSet =
    posSet
        |> Set.foldl (neighbours size >> Set.fromList >> Set.union) posSet
