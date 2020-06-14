module Board exposing (Board, State(..), cycleLabel, generate, openLid, toDict)

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import LidGrid as LG exposing (Lid)
import MineGrid as MG exposing (MineGrid)
import PosDict exposing (PosDict)
import Random exposing (Generator)
import Set


type Board
    = Board IntSize (PosDict Lid) MineGrid


generate : IntSize -> Generator Board
generate size =
    MG.generator size 0.1
        |> Random.map (Board size (PosDict.init size (always LG.Closed)))


type State
    = PlayerTurn
    | Lost


openLid : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLid pos (Board size lids mines) =
    case Maybe.map2 Tuple.pair (Dict.get pos lids) (MG.get mines pos) of
        Just ( LG.Closed, MG.Mine ) ->
            Just
                ( Lost
                , Board size (Dict.insert pos LG.Open lids) mines
                )

        Just ( LG.Closed, MG.Empty _ ) ->
            let
                nLidGrid =
                    MG.autoOpenPosSetFrom pos mines
                        |> Set.foldl
                            (\np ->
                                Dict.update np
                                    (Maybe.map
                                        (\lid ->
                                            if lid == LG.Closed then
                                                LG.Open

                                            else
                                                lid
                                        )
                                    )
                            )
                            lids
            in
            Just ( PlayerTurn, Board size (Dict.insert pos LG.Open lids) mines )

        _ ->
            Nothing


cycleLabel : ( Int, Int ) -> Board -> Maybe Board
cycleLabel pos (Board s l m) =
    let
        nl =
            Dict.update pos
                (Maybe.map
                    (\lid ->
                        case lid of
                            LG.Open ->
                                lid

                            LG.Closed ->
                                LG.Flagged

                            LG.Flagged ->
                                LG.Closed
                    )
                )
                l
    in
    if Dict.get pos l /= Just LG.Open then
        Board s nl m
            |> Just

    else
        Nothing


toDict : Board -> PosDict ( Lid, MG.Cell )
toDict (Board _ l m) =
    Dict.merge
        (\_ _ -> identity)
        (\k v1 v2 -> Dict.insert k ( v1, v2 ))
        (\_ _ -> identity)
        l
        (MG.toDict m)
        Dict.empty
