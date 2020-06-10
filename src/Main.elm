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
        cellCount =
            List.length gridPS

        minesGenerator =
            Random.list cellCount (Random.weighted ( 20, True ) [ ( 80, False ) ])
                |> Random.map (\bs -> List.map2 pair gridPS bs |> List.filter second |> List.map first)
    in
    Random.step minesGenerator (Random.initialSeed 100)
        |> first


gridWidth =
    6


gridHeight =
    12


cellWidth =
    70


gridPS =
    List.range 0 (gridWidth - 1)
        |> List.map (\x -> List.range 0 (gridHeight - 1) |> List.map (\y -> ( x, y )))
        |> List.concat


viewGrid =
    div
        [ styleWidth (gridWidth * cellWidth)
        , styleHeight (gridHeight * cellWidth)
        , relative
        ]
        (gridPS
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
        , style "overflow" "hidden"
        , style "outline" "1px solid dodgerblue"
        ]
        [ text " "

        --, text (Debug.toString p)
        , text " "
        , text
            (if List.member p mines then
                "***"

             else
                "___"
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
