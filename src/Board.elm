module Board exposing (Board, State(..), cycleLabel, generate, openLid, toDict)

import Dict exposing (Dict)
import Grid exposing (Grid)
import IntSize exposing (IntSize)
import Lid exposing (Lid)
import MineCell as Mines
import PosDict exposing (PosDict)
import Random exposing (Generator)
import Set


type Board
    = Board IntSize (Grid Lid) (Grid Mines.MineCell)


generate : IntSize -> Generator Board
generate size =
    Mines.generator size 0.1
        |> Random.map (Board size (Grid.filled size Lid.Closed))


type State
    = PlayerTurn
    | Lost


openLid : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLid pos (Board size lids mines) =
    case dictGet2 pos (Grid.toDict lids) (Mines.toDict mines) of
        Just ( Lid.Closed, Mines.Mine ) ->
            Just
                ( Lost
                , Board size (Grid.set pos Lid.Open lids) mines
                )

        Just ( Lid.Closed, Mines.Empty _ ) ->
            let
                nLids =
                    Set.foldl
                        lidOpenIfClosed
                        lids
                        (Mines.autoOpenPosSetFrom pos mines)
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


toDict : Board -> PosDict ( Lid, Mines.MineCell )
toDict (Board _ l m) =
    Dict.merge
        (\_ _ -> identity)
        (\k v1 v2 -> Dict.insert k ( v1, v2 ))
        (\_ _ -> identity)
        (Grid.toDict l)
        (Mines.toDict m)
        Dict.empty



-- Dict Helpers


dictGet2 : comparable -> Dict comparable v -> Dict comparable a -> Maybe ( v, a )
dictGet2 k a b =
    Maybe.map2 Tuple.pair (Dict.get k a) (Dict.get k b)
