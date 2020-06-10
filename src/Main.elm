module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import String exposing (fromFloat)
import Tuple exposing (first, pair, second)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { dict : Dict I2 Cover }


init =
    { dict = Dict.empty }


type Cover
    = Open


type Msg
    = Click I2


type alias I2 =
    ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click pos ->
            { model | dict = Dict.insert pos Open model.dict }


view m =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid m
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


viewGrid : Model -> Html Msg
viewGrid m =
    div
        [ styleWidth (gridWidth * cellWidth)
        , styleHeight (gridHeight * cellWidth)
        , relative
        ]
        (gridPS
            |> List.map (viewTile m)
        )


toScreenCords ( x, y ) =
    ( toFloat x * cellWidth, toFloat y * cellWidth )


viewTile : Model -> ( Int, Int ) -> Html Msg
viewTile m p =
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
        , onClick (Click p)
        ]
        [ case Dict.get p m.dict of
            Nothing ->
                text ""

            Just Open ->
                case isMine m p of
                    True ->
                        text "***"

                    False ->
                        text "N"
        ]


isMine : Model -> I2 -> Bool
isMine model loc =
    List.member loc mines



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
