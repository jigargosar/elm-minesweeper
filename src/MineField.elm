module MineField exposing (Cell(..), MineField, generator, get, getAutoOpenPositionsFrom)

import Dict
import IntSize exposing (IntSize)
import List.Extra as List
import More.Tuple as Tuple
import PosDict exposing (PosDict)
import Random exposing (Generator)
import Set exposing (Set)


type Cell
    = Mine
    | Empty Int


type MineField
    = MineField IntSize (PosDict Cell)


generator : ( Int, Int ) -> Float -> Generator MineField
generator size minePct =
    let
        intSize =
            IntSize.fromTuple size
    in
    minesGenerator size minePct
        |> Random.map (initCellDict intSize >> MineField intSize)


get : ( Int, Int ) -> MineField -> Maybe Cell
get k (MineField _ d) =
    Dict.get k d


getAutoOpenPositionsFrom : ( Int, Int ) -> MineField -> Set ( Int, Int )
getAutoOpenPositionsFrom pos (MineField size d) =
    connectedPositionsWithZeroSurroundingMines d pos Set.empty Set.empty
        |> IntSize.includeNeighboursOfEveryMember size


connectedPositionsWithZeroSurroundingMines :
    PosDict Cell
    -> ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
connectedPositionsWithZeroSurroundingMines grid current pending acc =
    let
        neighboursHavingZeroSurroundingMines =
            Tuple.neighboursOf current
                |> List.filter
                    (\neighbourPos ->
                        Dict.get neighbourPos grid == Just (Empty 0)
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
            connectedPositionsWithZeroSurroundingMines grid nCurrent (Set.fromList nPending) nAcc


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
