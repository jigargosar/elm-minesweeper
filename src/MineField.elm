module MineField exposing (Cell(..), MineField, generator, get, getAutoOpenPositionsFrom)

import Grid exposing (Grid)
import IntSize exposing (IntSize)
import List.Extra as List
import More.Tuple as Tuple
import Random exposing (Generator)
import Set exposing (Set)


type Cell
    = Mine
    | Empty Int


type MineField
    = MineField IntSize (Grid Cell)


generator : ( Int, Int ) -> Float -> Generator MineField
generator size minePct =
    minesGenerator size minePct
        |> Random.map (initCellGrid size >> MineField (IntSize.fromTuple size))


get : ( Int, Int ) -> MineField -> Maybe Cell
get k (MineField _ d) =
    Grid.get k d


getAutoOpenPositionsFrom : ( Int, Int ) -> MineField -> Set ( Int, Int )
getAutoOpenPositionsFrom pos (MineField size grid) =
    connectedZeroSurroundingMinesPositions grid pos Set.empty Set.empty
        |> IntSize.includeNeighboursOfEveryMember size


connectedZeroSurroundingMinesPositions grid current pending acc =
    let
        neighboursHavingZeroSurroundingMines =
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
        Set.diff neighboursHavingZeroSurroundingMines acc
            |> Set.union pending
            |> Set.toList
    of
        [] ->
            nAcc

        nCurrent :: nPending ->
            connectedZeroSurroundingMinesPositions grid nCurrent (Set.fromList nPending) nAcc


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
