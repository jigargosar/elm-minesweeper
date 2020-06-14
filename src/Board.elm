module Board exposing (Board, generate)

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import LidGrid as LG exposing (LidGrid)
import MineGrid exposing (MineGrid)
import Random exposing (Generator)


type alias Pos =
    ( Int, Int )


type alias PosDict a =
    Dict Pos a


type Board
    = Board IntSize LidGrid MineGrid


generate : IntSize -> Generator Board
generate size =
    MineGrid.generator size 0.1
        |> Random.map (Board size (LG.fillClosed size))
