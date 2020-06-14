module Board exposing (Board, State(..), cycleLabel, generator, openLidAt, toDict)

import Grid exposing (Grid)
import IntSize as Size exposing (IntSize)
import Lid exposing (Lid)
import List.Extra as List
import MineCell as Mine exposing (MineCell)
import More.Basics exposing (Int2Dict)
import Random exposing (Generator)
import Set exposing (Set)


type Board
    = Board (Grid ( Lid, MineCell ))


generator : IntSize -> Generator Board
generator size =
    minePosSetGenerator size 0.1
        |> Random.map (initMineCellGrid size >> Board)


type State
    = PlayerTurn
    | Lost


openLidAt : ( Int, Int ) -> Board -> Maybe ( State, Board )
openLidAt pos (Board grid) =
    case computeLidPositionsToOpen pos grid of
        Nothing ->
            Nothing

        Just ( state, toOpen ) ->
            Just
                ( state
                , Board
                    (Set.foldl
                        lidOpenIfClosed
                        grid
                        toOpen
                    )
                )


computeLidPositionsToOpen start grid =
    case Grid.get start grid of
        Nothing ->
            Nothing

        Just ( lid, mineCell ) ->
            if lid == Lid.Closed then
                case mineCell of
                    Mine.Mine ->
                        Just ( Lost, Set.singleton start )

                    Mine.Empty 0 ->
                        Just ( PlayerTurn, computeAutoOpenLidPositions grid (Set.singleton start) Set.empty )

                    Mine.Empty _ ->
                        Just ( PlayerTurn, Set.singleton start )

            else
                Nothing


computeAutoOpenLidPositions grid pending acc =
    case Set.toList pending of
        [] ->
            Grid.includeNeighbours grid acc
                |> Set.filter (\pos -> canOpenLidAt pos grid)

        current :: rest ->
            let
                toCompute =
                    Grid.neighbours current grid
                        |> List.filter (\( pos, _ ) -> canAutoOpenLidAt pos grid && not (Set.member pos acc))
                        |> List.map Tuple.first

                nPending =
                    Set.union (Set.fromList toCompute) (Set.fromList rest)
            in
            computeAutoOpenLidPositions grid nPending (Set.insert current acc)


canAutoOpenLidAt pos grid =
    case Grid.get pos grid of
        Just ( Lid.Closed, Mine.Empty 0 ) ->
            True

        _ ->
            False


canOpenLidAt pos grid =
    case Grid.get pos grid of
        Just ( Lid.Closed, _ ) ->
            True

        _ ->
            False


updateLid : ( Int, Int ) -> (Lid -> Lid) -> Grid ( Lid, MineCell ) -> Grid ( Lid, MineCell )
updateLid pos f =
    Grid.update pos (Tuple.mapFirst f)


lidOpenIfClosed pos =
    updateLid pos
        (\lid ->
            if lid == Lid.Closed then
                Lid.Open

            else
                lid
        )


cycleLabel : ( Int, Int ) -> Board -> Maybe Board
cycleLabel pos (Board grid) =
    case Grid.get pos grid of
        Just ( Lid.Open, _ ) ->
            Nothing

        _ ->
            Just
                (Board
                    (updateLid pos
                        (\lid ->
                            case lid of
                                Lid.Open ->
                                    lid

                                Lid.Closed ->
                                    Lid.Flagged

                                Lid.Flagged ->
                                    Lid.Closed
                        )
                        grid
                    )
                )


toDict : Board -> Int2Dict ( Lid, MineCell )
toDict (Board grid) =
    Grid.toDict grid



-- Mine Grid


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
                ( Lid.Closed, Mine.Mine )

            else
                ( Lid.Closed, Mine.Empty (neighbourMineCount pos) )
        )
