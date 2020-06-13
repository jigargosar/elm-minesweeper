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
import String exposing (fromFloat)
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
        ( mines, seed ) =
            let
                initialSeed =
                    flags.now
                        |> always 2
                        |> Random.initialSeed
            in
            Random.step minesGenerator initialSeed
    in
    ( { lids = LG.fillClosed gridSize
      , mines = mines
      , gameState = PlayerTurn
      , seed = seed
      }
        |> update (Click ( 0, 0 ))
        |> update (Click ( 4, 1 ))
    , Cmd.none
    )


minesGenerator =
    MG.generator gridSize 0.1


reset : Model -> Model
reset model =
    let
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

        ( Lost, Click _ ) ->
            reset model

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
    32


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
                    viewBaseTile pos "💣" [ backgroundColor "red" ]

                MG.Empty 0 ->
                    viewBaseTile pos "" []

                MG.Empty n ->
                    viewBaseTile pos (String.fromInt n) [ bold ]

        LG.Closed ->
            viewCoverTile pos "" []

        LG.Flagged ->
            div []
                [ viewCoverTile pos "⛳" [ color "red" ]
                , viewCoverTile pos "✔" [ color "green", backgroundColor "transparent" ]
                ]


type TileView
    = EmptyTile
    | NumTile Int
    | ClosedTile
    | FlaggedTile
    | MineTile
    | ExplodingMineTile


viewTile2 pos tile =
    case tile of
        EmptyTile ->
            emptyBaseTile pos

        NumTile n ->
            viewBaseTile pos (String.fromInt n) [ bold ]

        ClosedTile ->
            emptyCoverTile pos

        FlaggedTile ->
            viewCoverTile pos "⛳" [ color "red" ]

        MineTile ->
            viewBaseTile pos "💣" []

        ExplodingMineTile ->
            viewBaseTile pos "💣" [ backgroundColor "red" ]


bold =
    style "font-weight" "bold"


backgroundColor =
    style "background-color"


color =
    style "color"


emptyBaseTile pos =
    div (outerAttrs pos)
        [ div
            (innerAttrs
                ++ [ style "flex" "1 1 auto" ]
                ++ [ style "" ""
                   , style "border-style" "solid"
                   , style "border-width" "1px"
                   , style "border-color" (whitePct 80)
                   , backgroundColor (whitePct 90)
                   ]
            )
            []
        ]


emptyCoverTile pos =
    div (outerAttrs pos)
        [ div
            (innerAttrs
                ++ [ style "flex" "1 1 auto" ]
                ++ [ style "border-style" "outset"
                   , style "border-width" "4px"
                   , style "border-color" "unset"
                   , style "background-color" (whitePct 80)
                   ]
            )
            []
        ]


viewBaseTile pos string xs =
    div (outerAttrs pos)
        [ div
            (innerAttrs
                ++ [ style "" ""
                   , style "border-style" "solid"
                   , style "border-width" "1px"
                   , style "border-color" (whitePct 80)
                   , backgroundColor (whitePct 90)
                   ]
                ++ xs
            )
            [ text string ]
        ]


whitePct pct =
    "hsl(0,0%," ++ String.fromInt pct ++ "%)"


viewCoverTile pos string xs =
    div (outerAttrs pos)
        [ div
            (innerAttrs
                ++ [ style "border-style" "outset"
                   , style "border-width" "4px"
                   , style "border-color" "unset"
                   , style "background-color" (whitePct 80)
                   ]
                ++ xs
            )
            [ text string ]
        ]


outerAttrs pos =
    let
        sp =
            toScreenCords pos
    in
    [ styleWidth cellWidth
    , styleHeight cellWidth
    , absolute
    , transforms [ translate sp ]
    , style "overflow" "hidden"

    --, style "outline" "1px solid dodgerblue"
    , style "font-size" (String.fromFloat (cellWidth * 0.6) ++ "px")
    , style "font-family" "monospace"
    , style "display" "flex"

    --, style "align-items" "stretch"
    --, style "justify-content" "center"
    , style "user-select" "none"
    , onClick (Click pos)
    , E.preventDefaultOn "contextmenu" (JD.succeed ( RightClick pos, True ))
    ]


innerAttrs =
    [ style "" ""

    --, style "width" "100%"
    --, style "height" "100%"
    , style "flex" "1 1 auto"
    , style "overflow" "hidden"
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
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
