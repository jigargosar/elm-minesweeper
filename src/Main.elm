module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events as E exposing (onClick)
import IntSize as Size exposing (IntSize)
import Json.Decode as JD
import LidGrid exposing (Lid, LidGrid)
import MineGrid exposing (MineGrid)
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
    { lidGrid : LidGrid
    , gameState : GameState
    }


type GameState
    = PlayerTurn
    | Lost


type alias Loc =
    ( Int, Int )


init : Model
init =
    { lidGrid = LidGrid.fillClosed gridSize
    , gameState = PlayerTurn
    }


type Msg
    = Click Loc
    | RightClick Loc


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click loc ->
            case ( lidAt model loc, mineCellAt model loc ) of
                ( Just LidGrid.Closed, Just MineGrid.Mine ) ->
                    { model
                        | gameState = Lost
                        , lidGrid = LidGrid.open loc model.lidGrid
                    }

                ( Just LidGrid.Closed, Just (MineGrid.Empty _) ) ->
                    let
                        nLidGrid =
                            MineGrid.autoOpenPosSetFrom loc mines
                                |> Set.foldl LidGrid.openIfClosed model.lidGrid
                    in
                    { model | lidGrid = LidGrid.open loc nLidGrid }

                _ ->
                    model

        RightClick loc ->
            { model | lidGrid = LidGrid.cycleLabel loc model.lidGrid }


tileStateAt : Model -> ( Int, Int ) -> Maybe ( Lid, MineGrid.Cell )
tileStateAt model loc =
    Maybe.map2 Tuple.pair (lidAt model loc) (mineCellAt model loc)


lidAt : Model -> Loc -> Maybe Lid
lidAt model =
    LidGrid.get model.lidGrid


mineCellAt : Model -> Loc -> Maybe MineGrid.Cell
mineCellAt _ =
    MineGrid.get mines


view m =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid m
        ]


mines : MineGrid
mines =
    let
        minePct =
            0.1

        minesGenerator =
            MineGrid.generator gridSize minePct
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
            lidAt m loc == Just LidGrid.Open && mineCellAt m loc == Just MineGrid.Mine
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
        [ case lidAt m loc of
            Just LidGrid.Open ->
                text
                    (case mineCellAt m loc of
                        Nothing ->
                            ""

                        Just cell ->
                            case cell of
                                MineGrid.Mine ->
                                    "*"

                                MineGrid.Empty nmc ->
                                    fromInt nmc
                    )

            Just LidGrid.Flagged ->
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
