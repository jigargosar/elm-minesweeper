module MineField exposing (Cell(..), MineField, generator, get, zeroNeighbours)

import Dict
import Grid
import GridSize exposing (GridSize)
import List.Extra as List
import More.Tuple as Tuple
import Random exposing (Generator)
import Set exposing (Set)


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
generator ( w, h ) minePct =
    let
        size_ =
            ( w, h )

        size =
            GridSize.init w h
    in
    minePositionsGenerator size_ minePct
        |> Random.map (initCellDict size >> MineField size_)


get : I2 -> MineField -> Maybe Cell
get pos (MineField size d) =
    Dict.get pos d


zeroNeighbours : I2 -> MineField -> Set I2
zeroNeighbours position (MineField size d) =
    let
        nPos =
            Tuple.neighboursOf position
    in
    Dict.filter (\k v -> List.member k nPos && v == Empty 0) d
        |> Dict.keys
        |> Set.fromList


initCellDict : I2 -> Set I2 -> CellDict
initCellDict size minePosSet =
    let
        isMine pos =
            Set.member pos minePosSet

        neighbourMineCount pos =
            Tuple.neighboursOf pos |> List.count isMine
    in
    Grid.init size
        (\pos ->
            if Set.member pos minePosSet then
                Mine

            else
                Empty (neighbourMineCount pos)
        )
        |> Grid.toDict


minePositionsGenerator : I2 -> Float -> Generator (Set I2)
minePositionsGenerator size minePct =
    let
        xs =
            Tuple.range size
    in
    Random.list (List.length xs) (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
        |> Random.map
            (\boolList ->
                List.map2 Tuple.pair xs boolList
                    |> List.filter Tuple.second
                    |> List.map Tuple.first
                    |> Set.fromList
            )
