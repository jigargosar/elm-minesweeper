module MineField exposing (..)

import Dict
import Random exposing (Generator)


type alias I2 =
    ( Int, Int )


type Cell
    = Mine
    | Empty Int


type alias CellDict =
    Dict.Dict I2 Cell


type MineField
    = MineField I2 CellDict


generator : I2 -> Float -> Generator MineField
generator size minePct =
    minePositionsGenerator size minePct
        |> Random.map (initCellDict size >> MineField size)


initCellDict : I2 -> List I2 -> CellDict
initCellDict size minePositions =
    Debug.todo "impl"


minePositionsGenerator : I2 -> Float -> Generator (List I2)
minePositionsGenerator ( w, h ) minePct =
    let
        cellTotal =
            w * h

        positions =
            List.range 0 (w - 1)
                |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
                |> List.concat
    in
    Random.list cellTotal (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
        |> Random.map
            (\boolList ->
                List.map2 Tuple.pair positions boolList
                    |> List.filter Tuple.second
                    |> List.map Tuple.first
            )
