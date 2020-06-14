module MineCell exposing
    ( MineCell(..)
    , autoOpenPosSetFrom
    )

import Grid exposing (Grid)
import Set exposing (Set)


type MineCell
    = Mine
    | Empty Int


autoOpenPosSetFrom : ( Int, Int ) -> Grid MineCell -> Set ( Int, Int )
autoOpenPosSetFrom pos grid =
    if isEmptyWithNoSurroundingMines grid pos then
        connectedEmptyPositionsWithZeroSurroundingMines grid pos Set.empty Set.empty
            |> Grid.includeNeighboursPosSet grid

    else
        Set.empty


connectedEmptyPositionsWithZeroSurroundingMines :
    Grid MineCell
    -> ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
connectedEmptyPositionsWithZeroSurroundingMines grid current pending acc =
    let
        nAcc =
            Set.insert current acc
    in
    case
        Set.diff (neighboursHavingZeroSurroundingMines grid current) acc
            |> Set.union pending
            |> Set.toList
    of
        [] ->
            nAcc

        nCurrent :: nPending ->
            connectedEmptyPositionsWithZeroSurroundingMines grid nCurrent (Set.fromList nPending) nAcc


neighboursHavingZeroSurroundingMines : Grid MineCell -> ( Int, Int ) -> Set ( Int, Int )
neighboursHavingZeroSurroundingMines grid pos =
    Grid.neighbourPosSet grid pos
        |> Set.filter (isEmptyWithNoSurroundingMines grid)


isEmptyWithNoSurroundingMines dict pos =
    Grid.get pos dict == Just (Empty 0)
