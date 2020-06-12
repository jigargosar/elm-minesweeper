module More.Tuple exposing (..)


map : (a -> x) -> ( a, a ) -> ( x, x )
map f =
    Tuple.mapBoth f f


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


neighbours : List ( number, number )
neighbours =
    [ [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
    , [ ( -1, 0 ), ( 1, 0 ) ]
    , [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
    ]
        |> List.concat


add : ( number, number ) -> ( number, number ) -> ( number, number )
add =
    map2 (+)


neighboursOf : ( number, number ) -> List ( number, number )
neighboursOf xy =
    List.map (add xy) neighbours
