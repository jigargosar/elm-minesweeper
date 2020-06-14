module Board exposing (Board, State(..), cycleLabel, generate, openLid, toDict)

import Dict exposing (Dict)
import IntSize exposing (IntSize)
import LidGrid as LG exposing (Lid, LidGrid)
import MineGrid as MG exposing (MineGrid)
import Random exposing (Generator)
import Set


type alias Pos =
    ( Int, Int )


type alias PosDict a =
    Dict Pos a


type Board
    = Board IntSize LidGrid MineGrid


generate : IntSize -> Generator Board
generate size =
    MG.generator size 0.1
        |> Random.map (Board size (LG.fillClosed size))


type State
    = PlayerTurn
    | Lost


openLid : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLid pos (Board size lids mines) =
    case Maybe.map2 Tuple.pair (LG.get lids pos) (MG.get mines pos) of
        Just ( LG.Closed, MG.Mine ) ->
            Just
                ( Lost
                , Board size (LG.open pos lids) mines
                )

        Just ( LG.Closed, MG.Empty _ ) ->
            let
                nLidGrid =
                    MG.autoOpenPosSetFrom pos mines
                        |> Set.foldl LG.openIfClosed lids
            in
            Just ( PlayerTurn, Board size (LG.open pos nLidGrid) mines )

        _ ->
            Nothing


cycleLabel : ( Int, Int ) -> Board -> Maybe Board
cycleLabel pos (Board s l m) =
    let
        nl =
            LG.cycleLabelIfNotOpen pos l
    in
    if LG.get l pos /= Just LG.Open then
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
        (LG.toDict l)
        (MG.toDict m)
        Dict.empty
