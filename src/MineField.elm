module MineField exposing (Cell(..), MineField, generator, get, zeroNeighbours)

import Dict exposing (Dict)
import Grid
import List.Extra as List
import More.Tuple as Tuple
import Random exposing (Generator)
import Set exposing (Set)


type Cell
    = Mine
    | Empty Int


type alias CellDict =
    Dict ( Int, Int ) Cell


type MineField
    = MineField CellDict


generator : ( Int, Int ) -> Float -> Generator MineField
generator size minePct =
    minePositionsGenerator size minePct
        |> Random.map (initCellDict size >> MineField)


get : ( Int, Int ) -> MineField -> Maybe Cell
get pos (MineField d) =
    Dict.get pos d


zeroNeighbours : ( Int, Int ) -> MineField -> Set ( Int, Int )
zeroNeighbours position (MineField d) =
    let
        nPos =
            Tuple.neighboursOf position
    in
    Dict.filter (\k v -> List.member k nPos && v == Empty 0) d
        |> Dict.keys
        |> Set.fromList


initCellDict : ( Int, Int ) -> Set ( Int, Int ) -> CellDict
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


minePositionsGenerator : ( Int, Int ) -> Float -> Generator (Set ( Int, Int ))
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
