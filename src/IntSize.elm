module IntSize exposing (IntSize, fromTuple, init, isPosInvalid, isPosValid, neighbours)

import More.Tuple as Tuple


type IntSize
    = IntSize ( Int, Int )


init : Int -> Int -> IntSize
init w h =
    IntSize ( w, h )


fromTuple : ( Int, Int ) -> IntSize
fromTuple =
    IntSize


isPosInvalid : IntSize -> ( Int, Int ) -> Bool
isPosInvalid (IntSize ( w, h )) ( x, y ) =
    x < 0 || y < 0 || x >= w || y >= h


isPosValid : IntSize -> ( Int, Int ) -> Bool
isPosValid s =
    isPosInvalid s >> not


neighbours : IntSize -> ( Int, Int ) -> List ( Int, Int )
neighbours size pos =
    Tuple.neighboursOf pos
        |> List.filter (isPosValid size)
