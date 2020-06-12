module MineGrid exposing
    ( Cell(..)
    , MineGrid
    , generator
    , get
    , getAutoOpenPosSetFrom
    )

import Dict
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
    = MineGrid IntSize (PosDict Cell)


generator : IntSize -> Float -> Generator MineGrid
generator size minePct =
    minesGenerator size minePct
        |> Random.map (initCellDict size >> MineGrid size)


get : ( Int, Int ) -> MineGrid -> Maybe Cell
get k (MineGrid _ d) =
    Dict.get k d


getAutoOpenPosSetFrom : ( Int, Int ) -> MineGrid -> Set ( Int, Int )
getAutoOpenPosSetFrom pos ((MineGrid size grid) as model) =
    if hasNoSurroundingMines grid pos then
        connectedPositionsWithZeroSurroundingMines model pos Set.empty Set.empty
            |> Size.includeNeighbours size

    else
        Set.empty


connectedPositionsWithZeroSurroundingMines :
    MineGrid
    -> ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
connectedPositionsWithZeroSurroundingMines model current pending acc =
    let
        nAcc =
            Set.insert current acc
    in
    case
        Set.diff (neighboursHavingZeroSurroundingMines model current) acc
            |> Set.union pending
            |> Set.toList
    of
        [] ->
            nAcc

        nCurrent :: nPending ->
            connectedPositionsWithZeroSurroundingMines model nCurrent (Set.fromList nPending) nAcc


neighboursHavingZeroSurroundingMines (MineGrid size grid) pos =
    Size.neighbourSet size pos
        |> Set.filter (hasNoSurroundingMines grid)


hasNoSurroundingMines grid pos =
    Dict.get pos grid == Just (Empty 0)


initCellDict : IntSize -> Set ( Int, Int ) -> PosDict Cell
initCellDict size minePosSet =
    let
        isMine pos =
            Set.member pos minePosSet

        neighbourMineCount pos =
            Tuple.neighboursOf pos |> List.count isMine
    in
    PosDict.init size
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
