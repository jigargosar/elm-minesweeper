module GridSize exposing (GridSize, init, posSet)

import Set exposing (Set)


type alias I2 =
    ( Int, Int )


type GridSize
    = GridSize I2


init : Int -> Int -> GridSize
init w h =
    GridSize ( abs w, abs h )


posSet : GridSize -> Set ( Int, Int )
posSet (GridSize ( w, h )) =
    List.range 0 (w - 1)
        |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
        |> List.concat
        |> Set.fromList
