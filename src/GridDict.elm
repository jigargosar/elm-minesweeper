module GridDict exposing (GridDict)

import Dict exposing (Dict)
import GridSize exposing (GridSize)


type alias I2 =
    ( Int, Int )


type alias I2Dict a =
    Dict I2 a


type GridDict a
    = GridDict GridSize (I2Dict a)
