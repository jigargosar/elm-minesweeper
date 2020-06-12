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
    Browser.element
        { init = init
        , view = view
        , update = \msg -> update msg >> (\mo -> ( mo, Cmd.none ))
        , subscriptions = always Sub.none
        }


type alias Model =
    { lids : LidGrid
    , mines : MineGrid
    , gameState : GameState
    }


type GameState
    = PlayerTurn
    | Lost


type alias Loc =
    ( Int, Int )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        minesGenerator =
            MineGrid.generator gridSize 0.1

        mines : MineGrid
        mines =
            Random.step minesGenerator (Random.initialSeed 1)
                |> first
    in
    ( { lids = LidGrid.fillClosed gridSize
      , mines = mines
      , gameState = PlayerTurn
      }
    , Cmd.none
    )


type Msg
    = Click Loc
    | RightClick Loc


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click loc ->
            case tileAt model loc of
                Just ( LidGrid.Closed, MineGrid.Mine ) ->
                    { model
                        | gameState = Lost
                        , lids = LidGrid.open loc model.lids
                    }

                Just ( LidGrid.Closed, MineGrid.Empty _ ) ->
                    let
                        nLidGrid =
                            MineGrid.autoOpenPosSetFrom loc model.mines
                                |> Set.foldl LidGrid.openIfClosed model.lids
                    in
                    { model | lids = LidGrid.open loc nLidGrid }

                _ ->
                    model

        RightClick loc ->
            { model | lids = LidGrid.cycleLabel loc model.lids }


tileAt : Model -> ( Int, Int ) -> Maybe ( Lid, MineGrid.Cell )
tileAt model loc =
    Maybe.map2 Tuple.pair (lidAt model loc) (mineCellAt model loc)


lidAt : Model -> Loc -> Maybe Lid
lidAt model =
    LidGrid.get model.lids


mineCellAt : Model -> Loc -> Maybe MineGrid.Cell
mineCellAt model =
    MineGrid.get model.mines


view m =
    div []
        [ div [] [ text "MineSweeper" ]
        , viewGrid m
        ]


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
            tileAt m loc == Just ( LidGrid.Open, MineGrid.Mine )
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
        [ case tileAt m loc of
            Just ( LidGrid.Open, MineGrid.Mine ) ->
                text "*"

            Just ( LidGrid.Open, MineGrid.Empty n ) ->
                text (fromInt n)

            Just ( LidGrid.Flagged, _ ) ->
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
