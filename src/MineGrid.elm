module MineGrid exposing
    ( Cell(..)
    , MineGrid
    , autoOpenPosSetFrom
    , generator
    , toDict
    )

import Grid exposing (Grid)
import IntSize as Size exposing (IntSize)
import List.Extra as List
import More.Tuple as Tuple
import PosDict exposing (PosDict)
import Random exposing (Generator)
import Set exposing (Set)


type Cell
    = Mine
    | Empty Int


type MineGrid
    = MineGrid (Grid Cell)


toDict : MineGrid -> PosDict Cell
toDict (MineGrid g) =
    Grid.toDict g


generator : IntSize -> Float -> Generator MineGrid
generator size minePct =
    minesGenerator size minePct
        |> Random.map (initCellGrid size >> MineGrid)


autoOpenPosSetFrom : ( Int, Int ) -> MineGrid -> Set ( Int, Int )
autoOpenPosSetFrom pos (MineGrid grid) =
    if isEmptyWithNoSurroundingMines grid pos then
        connectedEmptyPositionsWithZeroSurroundingMines grid pos Set.empty Set.empty
            |> Grid.includeNeighboursPosSet grid

    else
        Set.empty


connectedEmptyPositionsWithZeroSurroundingMines :
    Grid Cell
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


neighboursHavingZeroSurroundingMines : Grid Cell -> ( Int, Int ) -> Set ( Int, Int )
neighboursHavingZeroSurroundingMines grid pos =
    Grid.neighbourPosSet grid pos
        |> Set.filter (isEmptyWithNoSurroundingMines grid)


isEmptyWithNoSurroundingMines dict pos =
    Grid.get pos dict == Just (Empty 0)


initCellGrid : IntSize -> Set ( Int, Int ) -> Grid Cell
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


minesGenerator : IntSize -> Float -> Generator (Set ( Int, Int ))
minesGenerator size minePct =
    let
        xs =
            Size.positions size
    in
    Random.list (List.length xs) (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
        |> Random.map
            (\boolList ->
                List.map2 Tuple.pair xs boolList
                    |> List.filter Tuple.second
                    |> List.map Tuple.first
                    |> Set.fromList
            )
