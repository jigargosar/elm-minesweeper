module Board exposing (Board, State(..), cycleLabel, generator, openLid, toDict)

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


type State
    = PlayerTurn
    | Lost


openLid : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLid pos (Board size lids mines) =
    case dictGet2 pos (Grid.toDict lids) (Grid.toDict mines) of
        Just ( Lid.Closed, Mine.Mine ) ->
            Just
                ( Lost
                , Board size (Grid.set pos Lid.Open lids) mines
                )

        Just ( Lid.Closed, Mine.Empty _ ) ->
            let
                nLids =
                    Set.foldl
                        lidOpenIfClosed
                        lids
                        (Mine.autoOpenPosSetFrom pos mines)
            in
            Just
                ( PlayerTurn
                , Board size (Grid.set pos Lid.Open nLids) mines
                )

        _ ->
            Nothing


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



-- Dict Helpers


dictGet2 : comparable -> Dict comparable v -> Dict comparable a -> Maybe ( v, a )
dictGet2 k a b =
    Maybe.map2 Tuple.pair (Dict.get k a) (Dict.get k b)
