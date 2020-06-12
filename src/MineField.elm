module MineField exposing (Cell(..), MineField, generator, get, getConnectedZeroCellPositions)

import Grid exposing (Grid)
import List.Extra as List
import More.Tuple as Tuple
import Random exposing (Generator)
import Set exposing (Set)


type Cell
    = Mine
    | Empty Int


type MineField
    = MineField (Grid Cell)


generator : ( Int, Int ) -> Float -> Generator MineField
generator size minePct =
    minesGenerator size minePct
        |> Random.map (initCellGrid size >> MineField)


get : ( Int, Int ) -> MineField -> Maybe Cell
get k (MineField d) =
    Grid.get k d


getAutoOpenPositionsFrom pos (MineField grid) =
    getConnectedZeroCellPositionsHelp grid pos Set.empty Set.empty
        |> includeNeighboursOfEveryMember


includeNeighboursOfEveryMember locSet =
    locSet
        |> Set.foldl (validNeighbours >> Set.union) locSet


validNeighbours loc =
    Tuple.neighboursOf loc
        |> Set.fromList
        |> Set.filter isValidPosition


isValidPosition =
    Debug.todo "impl"


getConnectedZeroCellPositions : ( Int, Int ) -> MineField -> Set ( Int, Int )
getConnectedZeroCellPositions pos (MineField g) =
    getConnectedZeroCellPositionsHelp g pos Set.empty Set.empty


getConnectedZeroCellPositionsHelp grid current pending acc =
    let
        neighboursWithZeroSurroundingMines =
            Tuple.neighboursOf current
                |> List.filter
                    (\neighbourPos ->
                        Grid.get neighbourPos grid == Just (Empty 0)
                    )
                |> Set.fromList

        nAcc =
            Set.insert current acc
    in
    case
        Set.diff neighboursWithZeroSurroundingMines acc
            |> Set.union pending
            |> Set.toList
    of
        [] ->
            nAcc

        nCurrent :: nPending ->
            getConnectedZeroCellPositionsHelp grid nCurrent (Set.fromList nPending) nAcc


initCellGrid : ( Int, Int ) -> Set ( Int, Int ) -> Grid Cell
initCellGrid size minePosSet =
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


minesGenerator : ( Int, Int ) -> Float -> Generator (Set ( Int, Int ))
minesGenerator size minePct =
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
