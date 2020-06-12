module GridSize exposing (GridSize, init)


type alias I2 =
    ( Int, Int )


type GridSize
    = GridSize I2


init : Int -> Int -> GridSize
init w h =
    GridSize ( abs w, abs h )
