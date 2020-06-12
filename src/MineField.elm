module MineField exposing (Cell(..), MineField, generator, get, zeroNeighbours)

import Dict
import GridSize exposing (GridSize)
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
generator (( w, h ) as size) minePct =
    minePositionsGenerator (GridSize.init w h) minePct
        |> Random.map (initCellDict size >> MineField size)


get : I2 -> MineField -> Maybe Cell
get pos (MineField size d) =
    Dict.get pos d


zeroNeighbours : I2 -> MineField -> Set I2
zeroNeighbours position (MineField size d) =
    let
        nPos =
            neighbourPositionsOf position
    in
    Dict.filter (\k v -> Set.member k nPos && v == Empty 0) d
        |> Dict.keys
        |> Set.fromList


initCellDict : I2 -> Set I2 -> CellDict
initCellDict size minePositions =
    let
        neighbourMineCount pos =
            neighbourPositionsOf pos
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


minePositionsGenerator : GridSize -> Float -> Generator (Set I2)
minePositionsGenerator size minePct =
    let
        posSet =
            GridSize.posSet size
    in
    Random.list (Set.size posSet) (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
        |> Random.map
            (\boolList ->
                List.map2 Tuple.pair (Set.toList posSet) boolList
                    |> List.filter Tuple.second
                    |> List.map Tuple.first
                    |> Set.fromList
            )


neighbourPositionsOf xy =
    Set.map (tupleMap2 (+) xy) unitNeighbours


tupleMap2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
tupleMap2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


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