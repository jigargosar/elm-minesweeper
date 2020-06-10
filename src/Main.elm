module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (style)
import String exposing (fromFloat)


main =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid
        ]


gw =
    10


gh =
    20


cw =
    100


spx s n =
    style s (fpx n)


fpx n =
    fromFloat n ++ "px"


sWidth =
    spx "width"


sHeight =
    spx "height"


relative =
    style "position" "relative"


absolute =
    style "position" "absolute"


gps =
    List.range 0 (gw - 1)
        |> List.map (\x -> List.range 0 (gh - 1) |> List.map (\y -> ( x, y )))
        |> List.concat


viewGrid =
    div
        [ sWidth (gw * cw)
        , sHeight (gh * cw)
        , relative
        ]
        (gps
            |> List.map viewTile
        )


toF2 =
    Tuple.mapBoth toFloat toFloat


translate x y =
    "translate(" ++ fpx x ++ "," ++ fpx y ++ ")"


toScreenCords ( x, y ) =
    ( toFloat x * cw, toFloat y * cw )


viewTile p =
    let
        ( sx, sy ) =
            toScreenCords p
    in
    div
        [ sWidth cw
        , sHeight cw
        , absolute
        , style "transform" (translate sx sy)
        ]
        [ text (Debug.toString p) ]
