module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events as E exposing (onClick)
import IntSize as Size exposing (IntSize)
import Json.Decode as JD
import MineField exposing (MineField)
import PosDict
import Random
import Set exposing (Set)
import String exposing (fromFloat, fromInt)
import Tuple exposing (first)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { tsDict : Dict Loc TileState
    , gameState : GameState
    }


type GameState
    = PlayerTurn
    | Lost


type alias Loc =
    ( Int, Int )


init : Model
init =
    { tsDict = PosDict.init gridSize (always Closed)
    , gameState = PlayerTurn
    }


type Msg
    = Click Loc
    | RightClick Loc


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click loc ->
            case tsAt model loc of
                Just s ->
                    case s of
                        Open ->
                            model

                        Closed ->
                            case MineField.get loc mines of
                                Nothing ->
                                    model

                                Just cell ->
                                    case cell of
                                        MineField.Mine ->
                                            { model
                                                | gameState = Lost
                                                , tsDict = Dict.insert loc Open model.tsDict
                                            }

                                        MineField.Empty _ ->
                                            let
                                                nts =
                                                    MineField.getAutoOpenPositionsFrom loc mines
                                                        |> Set.foldl
                                                            (\n ts ->
                                                                case Dict.get n ts of
                                                                    Just Closed ->
                                                                        Dict.insert n Open ts

                                                                    _ ->
                                                                        ts
                                                            )
                                                            model.tsDict
                                            in
                                            { model | tsDict = Dict.insert loc Open nts }

                        Flagged ->
                            model

                Nothing ->
                    model

        RightClick loc ->
            case tsAt model loc of
                Just s ->
                    case s of
                        Open ->
                            model

                        Closed ->
                            { model | tsDict = Dict.insert loc Flagged model.tsDict }

                        Flagged ->
                            { model | tsDict = Dict.insert loc Closed model.tsDict }

                Nothing ->
                    model


tsAt : Model -> Loc -> Maybe TileState
tsAt model loc =
    Dict.get loc model.tsDict


type TileState
    = Open
    | Closed
    | Flagged


view m =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid m
        ]


mines : MineField
mines =
    let
        minePct =
            0.1

        minesGenerator =
            MineField.generator gridSize minePct
    in
    Random.step minesGenerator (Random.initialSeed 1)
        |> first


gridSize : IntSize
gridSize =
    Size.init gridWidth gridHeight


gridWidth =
    6


gridHeight =
    12


cellWidth =
    70


viewGrid : Model -> Html Msg
viewGrid m =
    div
        [ styleWidth (gridWidth * cellWidth)
        , styleHeight (gridHeight * cellWidth)
        , relative
        ]
        (Size.positions gridSize
            |> List.map (viewTile m)
        )


toScreenCords ( x, y ) =
    ( toFloat x * cellWidth, toFloat y * cellWidth )


viewTile : Model -> ( Int, Int ) -> Html Msg
viewTile m loc =
    let
        sp =
            toScreenCords loc

        isOpenMine =
            tsAt m loc == Just Open && MineField.get loc mines == Just MineField.Mine
    in
    div
        ([ styleWidth cellWidth
         , styleHeight cellWidth
         , absolute
         , transforms [ translate sp ]
         , style "overflow" "hidden"
         , style "outline" "1px solid dodgerblue"
         , style "font-size" "3rem"
         , style "font-family" "monospace"
         , style "display" "flex"
         , style "align-items" "center"
         , style "justify-content" "center"
         , onClick (Click loc)
         , E.preventDefaultOn "contextmenu" (JD.succeed ( RightClick loc, True ))
         , style "user-select" "none"
         ]
            ++ (if isOpenMine then
                    [ style "background-color" "red"
                    ]

                else
                    []
               )
        )
        [ case tsAt m loc of
            Just Open ->
                text
                    (case MineField.get loc mines of
                        Nothing ->
                            ""

                        Just cell ->
                            case cell of
                                MineField.Mine ->
                                    "*"

                                MineField.Empty nmc ->
                                    fromInt nmc
                    )

            Just Flagged ->
                text "F"

            _ ->
                text ""
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
