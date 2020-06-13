module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events as E exposing (onClick)
import IntSize as Size exposing (IntSize)
import Json.Decode as JD
import LidGrid exposing (Lid, LidGrid)
import MineGrid exposing (MineGrid)
import Random exposing (Seed)
import Set exposing (Set)
import String exposing (fromFloat, fromInt)
import Tuple


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
    , seed : Seed
    }


type GameState
    = PlayerTurn
    | Lost


type alias Pos =
    ( Int, Int )


init : { now : Int } -> ( Model, Cmd Msg )
init flags =
    let
        minesGenerator =
            MineGrid.generator gridSize 0.1

        ( mines, seed ) =
            Random.step minesGenerator (Random.initialSeed flags.now)
    in
    ( { lids = LidGrid.fillClosed gridSize
      , mines = mines
      , gameState = PlayerTurn
      , seed = seed
      }
    , Cmd.none
    )


reset : Model -> Model
reset model =
    let
        minesGenerator =
            MineGrid.generator gridSize 0.1

        ( mines, seed ) =
            Random.step minesGenerator model.seed
    in
    { lids = LidGrid.fillClosed gridSize
    , mines = mines
    , gameState = PlayerTurn
    , seed = seed
    }


type Msg
    = Click Pos
    | RightClick Pos
    | ResetClicked


update : Msg -> Model -> Model
update msg model =
    case ( model.gameState, msg ) of
        ( PlayerTurn, Click loc ) ->
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

        ( PlayerTurn, RightClick loc ) ->
            { model | lids = LidGrid.cycleLabel loc model.lids }

        ( _, ResetClicked ) ->
            reset model

        _ ->
            model


tileAt : Model -> ( Int, Int ) -> Maybe ( Lid, MineGrid.Cell )
tileAt model pos =
    Maybe.map2 Tuple.pair (lidAt model pos) (mineCellAt model pos)


tileEntryAt : Model -> ( Int, Int ) -> Maybe ( ( Int, Int ), ( Lid, MineGrid.Cell ) )
tileEntryAt model pos =
    tileAt model pos |> Maybe.map (Tuple.pair pos)


tileEntries : Model -> List ( ( Int, Int ), ( Lid, MineGrid.Cell ) )
tileEntries model =
    Size.positions gridSize
        |> List.filterMap (tileEntryAt model)


lidAt : Model -> Pos -> Maybe Lid
lidAt model =
    LidGrid.get model.lids


mineCellAt : Model -> Pos -> Maybe MineGrid.Cell
mineCellAt model =
    MineGrid.get model.mines


view m =
    div []
        [ div [] [ text <| "MineSweeper: " ++ Debug.toString m.gameState ]
        , div [] [ button [ onClick ResetClicked ] [ text "Reset" ] ]
        , viewGrid m
        ]


gridSize : IntSize
gridSize =
    Size.init gridWidth gridHeight


gridWidth =
    16


gridHeight =
    16


cellWidth =
    40


viewGrid : Model -> Html Msg
viewGrid model =
    div
        [ styleWidth (gridWidth * cellWidth)
        , styleHeight (gridHeight * cellWidth)
        , relative
        ]
        (model |> tileEntries |> List.map viewTile)


toScreenCords ( x, y ) =
    ( toFloat x * cellWidth, toFloat y * cellWidth )


viewTile ( loc, tile ) =
    let
        isOpenMine =
            tile == ( LidGrid.Open, MineGrid.Mine )
    in
    div
        (commonTileAttrs loc
            ++ (if isOpenMine then
                    [ style "background-color" "red"
                    ]

                else
                    []
               )
        )
        [ case tile of
            ( LidGrid.Open, MineGrid.Mine ) ->
                text "*"

            ( LidGrid.Open, MineGrid.Empty n ) ->
                text (fromInt n)

            ( LidGrid.Flagged, _ ) ->
                text "F"

            _ ->
                text ""
        ]


commonTileAttrs : ( Int, Int ) -> List (Attribute Msg)
commonTileAttrs loc =
    let
        sp =
            toScreenCords loc
    in
    [ styleWidth cellWidth
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
