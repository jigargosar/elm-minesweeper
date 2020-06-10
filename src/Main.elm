module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (style)
import Random
import String exposing (fromFloat)
import Tuple exposing (first, pair, second)


main =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid
        ]


mines : List ( Int, Int )
mines =
    let
        l =
            List.length gps

        minesGenerator =
            Random.list l (Random.weighted ( 10, True ) [ ( 90, False ) ])
                |> Random.map (\bs -> List.map2 pair gps bs |> List.filter second |> List.map first)
    in
    Random.step minesGenerator (Random.initialSeed 100)
        |> first


gridWidth =
    10


gridHeight =
    20


cellWidth =
    50


gps =
    List.range 0 (gridWidth - 1)
        |> List.map (\x -> List.range 0 (gridHeight - 1) |> List.map (\y -> ( x, y )))
        |> List.concat


viewGrid =
    div
        [ styleWidth (gridWidth * cellWidth)
        , styleHeight (gridHeight * cellWidth)
        , relative
        ]
        (gps
            |> List.map viewTile
        )


toScreenCords ( x, y ) =
    ( toFloat x * cellWidth, toFloat y * cellWidth )


viewTile p =
    let
        sp =
            toScreenCords p
    in
    div
        [ styleWidth cellWidth
        , styleHeight cellWidth
        , absolute
        , transforms [ translate sp ]
        ]
        [ text (Debug.toString p)
        , text
            (if List.member p mines then
                "Mine"

             else
                "Empty"
            )
        ]



-- Style Helpers


transforms xs =
    style "transform" (String.join " " xs)


stylePx s n =
    style s (floatPx n)


floatPx n =
    fromFloat n ++ "px"


styleWidth =
    stylePx "width"


styleHeight =
    stylePx "height"


relative =
    style "position" "relative"


absolute =
    style "position" "absolute"


translate ( x, y ) =
    "translate(" ++ floatPx x ++ "," ++ floatPx y ++ ")"
