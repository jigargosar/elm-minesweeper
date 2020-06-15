module Board exposing (Board, State(..), cycleLabel, generator, openLidAt, toDict)

import Grid exposing (Grid)
import IntSize as Size exposing (IntSize)
import Lid exposing (Lid)
import List.Extra as List
import MineCell as Mine exposing (MineCell)
import More.Basics exposing (Int2, Int2Dict)
import Random exposing (Generator)
import Set exposing (Set)


type Board
    = Board CellGrid


type alias Cell =
    ( Lid, MineCell )


type alias CellGrid =
    Grid Cell


generator : IntSize -> Generator Board
generator size =
    minePosSetGenerator size 0.1
        |> Random.map (initCellGrid size >> Board)


type State
    = PlayerTurn
    | Lost


openLidAt : Int2 -> Board -> Maybe ( State, Board )
openLidAt input (Board grid) =
    case computePositionsToOpen input grid of
        Nothing ->
            Nothing

        Just ( state, toOpen ) ->
            Just
                ( state
                , Board
                    (Set.foldl
                        (\pos -> setLid pos Lid.Open)
                        grid
                        toOpen
                    )
                )


computePositionsToOpen : Int2 -> CellGrid -> Maybe ( State, Set Int2 )
computePositionsToOpen start grid =
    case cellAt start grid of
        Just ( Lid.Closed, mineCell ) ->
            case mineCell of
                Mine.Mine ->
                    Just
                        ( Lost
                        , Set.singleton start
                        )

                Mine.Empty neighbourMineCount ->
                    Just
                        ( PlayerTurn
                        , if neighbourMineCount == 0 then
                            computeAutoOpenPositions
                                grid
                                (Set.singleton start)
                                Set.empty

                          else
                            Set.singleton start
                        )

        _ ->
            Nothing


computeAutoOpenPositions : CellGrid -> Set Int2 -> Set Int2 -> Set Int2
computeAutoOpenPositions grid pending result =
    case Set.toList pending of
        [] ->
            Set.foldl
                (\pos ->
                    Set.union
                        (neighbourPositionsWhere (\_ -> canOpenCell) pos grid)
                )
                result
                result

        current :: rest ->
            let
                toCompute =
                    neighbourPositionsWhere
                        (\nPos nCell ->
                            canAutoOpenCell nCell
                                && not (Set.member nPos result)
                        )
                        current
                        grid

                nPending =
                    Set.union toCompute (Set.fromList rest)
            in
            computeAutoOpenPositions grid nPending (Set.insert current result)


neighbourPositionsWhere : (Int2 -> b -> Bool) -> Int2 -> Grid b -> Set Int2
neighbourPositionsWhere isOk pos grid =
    Grid.neighbours pos grid
        |> List.filter (\( neighbourPos, neighbourCell ) -> isOk neighbourPos neighbourCell)
        |> List.map Tuple.first
        |> Set.fromList


canOpenCell : Cell -> Bool
canOpenCell ( lid, _ ) =
    lid == Lid.Closed


canAutoOpenCell : Cell -> Bool
canAutoOpenCell cell =
    cell == ( Lid.Closed, Mine.Empty 0 )


cellAt : Int2 -> CellGrid -> Maybe Cell
cellAt =
    Grid.get


updateLid : Int2 -> (Lid -> Lid) -> CellGrid -> CellGrid
updateLid pos f =
    Grid.update pos (Tuple.mapFirst f)


setLid : Int2 -> Lid -> CellGrid -> CellGrid
setLid pos lid =
    updateLid pos (always lid)


cycleLabel : Int2 -> Board -> Maybe Board
cycleLabel pos (Board grid) =
    let
        maybeLid =
            case cellAt pos grid |> Maybe.map Tuple.first of
                Just Lid.Closed ->
                    Just Lid.Flagged

                Just Lid.Flagged ->
                    Just Lid.Closed

                _ ->
                    Nothing
    in
    maybeLid
        |> Maybe.map
            (\lid ->
                Board (setLid pos lid grid)
            )


toDict : Board -> Int2Dict Cell
toDict (Board grid) =
    Grid.toDict grid


minePosSetGenerator : IntSize -> Float -> Generator (Set Int2)
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


initCellGrid : IntSize -> Set Int2 -> CellGrid
initCellGrid size minePosSet =
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
