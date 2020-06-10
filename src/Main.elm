module Main exposing (main)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Random
import String exposing (fromFloat)
import Tuple exposing (first, pair, second)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init =
    {}


type Msg
    = Click


update msg model =
    case msg of
        Click ->
            model


view _ =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid
        ]


mines : List ( Int, Int )
mines =
    let
        minePct =
            0.1

        minesGenerator =
            Random.list (List.length gridPS) (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
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
