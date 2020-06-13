module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events as E exposing (onClick)
import IntSize as Size exposing (IntSize)
import Json.Decode as JD
import LidGrid as LG exposing (Lid, LidGrid)
import MineGrid as MG exposing (MineGrid)
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
            MG.generator gridSize 0.1

        ( mines, seed ) =
            Random.step minesGenerator (Random.initialSeed flags.now)
    in
    ( { lids = LG.fillClosed gridSize
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
            MG.generator gridSize 0.1

        ( mines, seed ) =
            Random.step minesGenerator model.seed
    in
    { lids = LG.fillClosed gridSize
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
                Just ( LG.Closed, MG.Mine ) ->
                    { model
                        | gameState = Lost
                        , lids = LG.open loc model.lids
                    }

                Just ( LG.Closed, MG.Empty _ ) ->
                    let
                        nLidGrid =
                            MG.autoOpenPosSetFrom loc model.mines
                                |> Set.foldl LG.openIfClosed model.lids
                    in
                    { model | lids = LG.open loc nLidGrid }

                _ ->
                    model

        ( PlayerTurn, RightClick loc ) ->
            { model | lids = LG.cycleLabel loc model.lids }

        ( _, ResetClicked ) ->
            reset model

        _ ->
            model


tileAt : Model -> ( Int, Int ) -> Maybe ( Lid, MG.Cell )
tileAt model pos =
    Maybe.map2 Tuple.pair (lidAt model pos) (mineCellAt model pos)


tileEntryAt : Model -> ( Int, Int ) -> Maybe ( ( Int, Int ), ( Lid, MG.Cell ) )
tileEntryAt model pos =
    tileAt model pos |> Maybe.map (Tuple.pair pos)


tileEntries : Model -> List ( ( Int, Int ), ( Lid, MG.Cell ) )
tileEntries model =
    Size.positions gridSize
        |> List.filterMap (tileEntryAt model)


lidAt : Model -> Pos -> Maybe Lid
lidAt model =
    LG.get model.lids


mineCellAt : Model -> Pos -> Maybe MG.Cell
mineCellAt model =
    MG.get model.mines


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


viewTile ( pos, ( lid, cell ) ) =
    case lid of
        LG.Open ->
            case cell of
                MG.Mine ->
                    viewMineTile pos

                MG.Empty 0 ->
                    viewEmptyTile pos

                MG.Empty n ->
                    viewNumTile pos n

        LG.Closed ->
            viewClosedTile pos

        LG.Flagged ->
            viewFlaggedTile pos


viewTile_ ( pos, tile ) =
    let
        isOpenMine =
            tile == ( LG.Open, MG.Mine )
    in
    div
        (commonTileAttrs pos
            ++ (if isOpenMine then
                    [ style "background-color" "red"
                    ]

                else
                    []
               )
        )
        [ case tile of
            ( LG.Open, MG.Mine ) ->
                text "*"

            ( LG.Open, MG.Empty n ) ->
                text (fromInt n)

            ( LG.Flagged, _ ) ->
                text "F"

            _ ->
                text ""
        ]


viewExplodedMineTile pos =
    div
        (commonTileAttrs pos
            ++ [ style "background-color" "red"
               ]
        )
        [ text "*" ]


viewMineTile pos =
    div
        (commonTileAttrs pos)
        [ text "*" ]


viewNumTile pos n =
    div
        (commonTileAttrs pos)
        [ text (String.fromInt n) ]


viewEmptyTile pos =
    div
        (commonTileAttrs pos)
        [ text "" ]


viewClosedTile pos =
    div
        (commonTileAttrs pos)
        [ text "" ]


viewFlaggedTile pos =
    div
        (commonTileAttrs pos)
        [ text "F" ]


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
