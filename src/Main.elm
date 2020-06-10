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


gps =
    List.range 0 (gw - 1)
        |> List.map (\x -> List.range 0 (gh - 1) |> List.map (\y -> ( x, y )))
        |> List.concat


viewGrid =
    div
        [ widthPx (gw * cw)
        , heightPx (gh * cw)
        , relative
        ]
        (gps
            |> List.map viewTile
        )


toScreenCords ( x, y ) =
    ( toFloat x * cw, toFloat y * cw )


viewTile p =
    let
        sp =
            toScreenCords p
    in
    div
        [ widthPx cw
        , heightPx cw
        , absolute
        , transforms [ translate sp ]
        ]
        [ text (Debug.toString p) ]



-- Style Helpers


transforms xs =
    style "transform" (String.join " " xs)


stylePx s n =
    style s (floatPx n)


floatPx n =
    fromFloat n ++ "px"


widthPx =
    stylePx "width"


heightPx =
    stylePx "height"


relative =
    style "position" "relative"


absolute =
    style "position" "absolute"


translate ( x, y ) =
    "translate(" ++ floatPx x ++ "," ++ floatPx y ++ ")"
