module Board exposing (Board, State(..), cycleLabel, generator, openLidAt, toDict)

import Dict exposing (Dict)
import Grid exposing (Grid)
import IntSize as Size exposing (IntSize)
import Lid exposing (Lid)
import List.Extra as List
import MineCell as Mine exposing (MineCell)
import PosDict exposing (PosDict)
import Random exposing (Generator)
import Set exposing (Set)


type Board
    = Board IntSize (Grid Lid) (Grid MineCell)


generator : IntSize -> Generator Board
generator size =
    mineGridGenerator size 0.1
        |> Random.map (Board size (Grid.filled size Lid.Closed))


type State
    = PlayerTurn
    | Lost


openLidAt : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLidAt pos (Board size lids mines) =
    case computeLidPositionsToOpen pos lids mines of
        Nothing ->
            Nothing

        Just ( state, toOpen ) ->
            Just
                ( state
                , Board size
                    (Set.foldl
                        lidOpenIfClosed
                        lids
                        toOpen
                    )
                    mines
                )


computeLidPositionsToOpen start lids mines =
    if lidsCanOpenAt start lids then
        case Grid.get start mines of
            Nothing ->
                Nothing

            Just mineCell ->
                case mineCell of
                    Mine.Mine ->
                        Just ( Lost, Set.singleton start )

                    Mine.Empty 0 ->
                        Just ( PlayerTurn, computeAutoOpenLidPositions lids mines (Set.singleton start) Set.empty )

                    Mine.Empty _ ->
                        Just ( PlayerTurn, Set.singleton start )

    else
        Nothing


computeAutoOpenLidPositions : Grid Lid -> Grid MineCell -> Set ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
computeAutoOpenLidPositions lids mines pending acc =
    case Set.toList pending of
        [] ->
            Grid.includeNeighbours mines acc
                |> Set.filter (\pos -> lidsCanOpenAt pos lids)

        current :: rest ->
            let
                toCompute =
                    Grid.neighbourSet current mines
                        |> Set.filter (\pos -> canAutoOpenLidAt pos lids mines && not (Set.member pos acc))

                nPending =
                    Set.union toCompute (Set.fromList rest)
            in
            computeAutoOpenLidPositions lids mines nPending (Set.insert current acc)


canAutoOpenLidAt pos lids mines =
    lidsCanOpenAt pos lids && Grid.get pos mines == Just (Mine.Empty 0)


lidsCanOpenAt pos lids =
    Grid.get pos lids == Just Lid.Closed


lidOpenIfClosed : ( Int, Int ) -> Grid Lid -> Grid Lid
lidOpenIfClosed pos =
    Grid.update pos
        (\lid ->
            if lid == Lid.Closed then
                Lid.Open

            else
                lid
        )


cycleLabel : ( Int, Int ) -> Board -> Maybe Board
cycleLabel pos (Board s l m) =
    let
        nl =
            Grid.update pos
                (\lid ->
                    case lid of
                        Lid.Open ->
                            lid

                        Lid.Closed ->
                            Lid.Flagged

                        Lid.Flagged ->
                            Lid.Closed
                )
                l
    in
    if Grid.get pos l /= Just Lid.Open then
        Board s nl m
            |> Just

    else
        Nothing


toDict : Board -> PosDict ( Lid, MineCell )
toDict (Board _ l m) =
    Dict.merge
        (\_ _ -> identity)
        (\k v1 v2 -> Dict.insert k ( v1, v2 ))
        (\_ _ -> identity)
        (Grid.toDict l)
        (Grid.toDict m)
        Dict.empty



-- Mine Grid


mineGridGenerator : IntSize -> Float -> Generator (Grid MineCell)
mineGridGenerator size minePct =
    minePosSetGenerator size minePct
        |> Random.map (initMineCellGrid size)


minePosSetGenerator : IntSize -> Float -> Generator (Set ( Int, Int ))
minePosSetGenerator size minePct =
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


initMineCellGrid : IntSize -> Set ( Int, Int ) -> Grid MineCell
initMineCellGrid size minePosSet =
    let
        isMine pos =
            Set.member pos minePosSet

        neighbourMineCount pos =
            Size.neighbours size pos |> List.count isMine
    in
    Grid.init size
        (\pos ->
            if Set.member pos minePosSet then
                Mine.Mine

            else
                Mine.Empty (neighbourMineCount pos)
        )
