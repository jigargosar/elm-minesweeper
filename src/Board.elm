module Board exposing (Board, State(..), cycleLabel, generate, openLid, toDict)

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import Lid exposing (Lid)
import MineGrid as Mines exposing (MineGrid)
import PosDict exposing (PosDict)
import Random exposing (Generator)
import Set


type Board
    = Board IntSize (PosDict Lid) MineGrid


generate : IntSize -> Generator Board
generate size =
    Mines.generator size 0.1
        |> Random.map (Board size (PosDict.filled size Lid.Closed))


type State
    = PlayerTurn
    | Lost


openLid : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLid pos (Board size lids mines) =
    case dictGet2 pos lids (Mines.toDict mines) of
        Just ( Lid.Closed, Mines.Mine ) ->
            Just
                ( Lost
                , Board size (dictSetExisting pos Lid.Open lids) mines
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
                , Board size (dictSetExisting pos Lid.Open nLids) mines
                )

        _ ->
            Nothing


lidOpenIfClosed : comparable -> Dict comparable Lid -> Dict comparable Lid
lidOpenIfClosed pos =
    dictUpdateExisting pos
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
            dictUpdateExisting pos
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
    if Dict.get pos l /= Just Lid.Open then
        Board s nl m
            |> Just

    else
        Nothing


toDict : Board -> PosDict ( Lid, Mines.Cell )
toDict (Board _ l m) =
    Dict.merge
        (\_ _ -> identity)
        (\k v1 v2 -> Dict.insert k ( v1, v2 ))
        (\_ _ -> identity)
        l
        (Mines.toDict m)
        Dict.empty



-- Dict Helpers


dictUpdateExisting : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdateExisting k f =
    Dict.update k (Maybe.map f)


dictGet2 : comparable -> Dict comparable v -> Dict comparable a -> Maybe ( v, a )
dictGet2 k a b =
    Maybe.map2 Tuple.pair (Dict.get k a) (Dict.get k b)


dictSetExisting : comparable -> b -> Dict comparable b -> Dict comparable b
dictSetExisting k v =
    dictUpdateExisting k (always v)
