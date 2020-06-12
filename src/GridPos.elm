module GridPos exposing (GridPos, init)


type alias I2 =
    ( Int, Int )


type GridPos
    = GridPos I2


init : Int -> Int -> GridPos
init x y =
    GridPos ( x, y )
