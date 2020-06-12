module MineField exposing (..)

import Dict
import Random exposing (Generator)
import Set exposing (Set)


type alias I2 =
    ( Int, Int )


type Cell
    = Mine
    | Empty Int


type alias CellDict =
    Dict.Dict I2 Cell


type MineField
    = MineField I2 CellDict


generator : I2 -> Float -> Generator MineField
generator size minePct =
    minePositionsGenerator size minePct
        |> Random.map (initCellDict size >> MineField size)


initCellDict : I2 -> Set I2 -> CellDict
initCellDict size minePositions =
    let
        neighbourMineCount pos =
            neighbourOf pos
                |> Set.foldl
                    (\nPos count ->
                        if Set.member nPos minePositions then
                            count + 1

                        else
                            count
                    )
                    0
    in
    Set.foldl
        (\pos ->
            Dict.insert pos
                (if Set.member pos minePositions then
                    Mine

                 else
                    Empty (neighbourMineCount pos)
                )
        )
        Dict.empty
        (positionsFromSize size)


validNeighboursInSize : I2 -> I2 -> Set I2
validNeighboursInSize size pos =
    neighbourOf pos
        |> Set.filter (isValidInSize size)


isValidInSize : I2 -> I2 -> Bool
isValidInSize ( w, h ) ( x, y ) =
    not (x < 0 || y < 0 || x >= w || y >= h)


neighbourOf ( x, y ) =
    Set.map (\( dx, dy ) -> ( x + dx, y + dy )) unitNeighbours


unitNeighbours =
    [ [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
    , [ ( -1, 0 ), ( 1, 0 ) ]
    , [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
    ]
        |> List.concat
        |> Set.fromList


positionsFromSize : I2 -> Set I2
positionsFromSize ( w, h ) =
    List.range 0 (w - 1)
        |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
        |> List.concat
        |> Set.fromList


minePositionsGenerator : I2 -> Float -> Generator (Set I2)
minePositionsGenerator ( w, h ) minePct =
    let
        cellTotal =
            w * h

        positions =
            List.range 0 (w - 1)
                |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
                |> List.concat
    in
    Random.list cellTotal (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
        |> Random.map
            (\boolList ->
                List.map2 Tuple.pair positions boolList
                    |> List.filter Tuple.second
                    |> List.map Tuple.first
                    |> Set.fromList
            )
