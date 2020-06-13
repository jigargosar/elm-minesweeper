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
    | Lost ( Int, Int )


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
        |> update (RightClick ( 4, 0 ))
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
                        | gameState = Lost loc
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

        ( Lost _, Click _ ) ->
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
        (case model.gameState of
            PlayerTurn ->
                model
                    |> tileEntries
                    |> List.map
                        (\( pos, ( lid, cell ) ) ->
                            renderTileView pos (toPlayerTurnTileView lid cell)
                        )

            Lost _ ->
                model
                    |> tileEntries
                    |> List.map
                        (\( pos, ( lid, cell ) ) ->
                            renderTileView pos (toLostTileView lid cell)
                        )
        )


toPlayerTurnTileView lid cell =
    case lid of
        LG.Open ->
            case cell of
                MG.Mine ->
                    MineView

                MG.Empty 0 ->
                    EmptyView

                MG.Empty n ->
                    NumView n

        LG.Closed ->
            ClosedView

        LG.Flagged ->
            FlagView


toLostTileView lid cell =
    case cell of
        MG.Mine ->
            case lid of
                LG.Open ->
                    ExplodingMineView

                LG.Closed ->
                    MineView

                LG.Flagged ->
                    SuccessMineView

        MG.Empty 0 ->
            case lid of
                LG.Open ->
                    EmptyView

                LG.Closed ->
                    EmptyView

                LG.Flagged ->
                    FailureEmptyView

        MG.Empty n ->
            case lid of
                LG.Open ->
                    NumView n

                LG.Closed ->
                    NumView n

                LG.Flagged ->
                    FailureNumView n


toScreenCords ( x, y ) =
    ( toFloat x * cellWidth, toFloat y * cellWidth )


type TileView
    = MineView
    | ExplodingMineView
    | EmptyView
    | NumView Int
    | ClosedView
    | FlagView
    | SuccessMineView
    | FailureEmptyView
    | FailureNumView Int


renderTileView pos tv =
    case tv of
        MineView ->
            div []
                [ emptyBaseTile pos [], mineTile pos ]

        ExplodingMineView ->
            div []
                [ emptyBaseTile pos [ backgroundColor "red", opacity 0.5 ], mineTile pos ]

        SuccessMineView ->
            div []
                [ emptyBaseTile pos [], mineTile pos, tickTile pos ]

        FailureEmptyView ->
            div [] [ emptyBaseTile pos [], crossTile pos ]

        FailureNumView n ->
            div [] [ emptyBaseTile pos [], numTile pos n, crossTile pos ]

        EmptyView ->
            div [] [ emptyBaseTile pos [] ]

        NumView n ->
            div [] [ emptyBaseTile pos [], numTile pos n ]

        ClosedView ->
            div [] [ emptyCoverTile pos ]

        FlagView ->
            div []
                [ emptyCoverTile pos
                , stringTile pos "⛳" [ color "blue" ]
                ]


numTile pos n =
    stringTile pos (String.fromInt n) []


crossTile pos =
    stringTile pos "❌" [ color "red", bold, opacity 0.6 ]


tickTile pos =
    stringTile pos "✔" [ color "green" ]


mineTile pos =
    stringTile pos "💣" [ opacity 0.8 ]


emptyBaseTile pos xs =
    div
        (commonAttrs pos
            ++ [ borderSolid
               , borderWidth 0.5
               , borderColor (whitePct 80)
               , backgroundColor (whitePct 90)
               ]
            ++ xs
        )
        []


emptyCoverTile pos =
    div
        (commonAttrs pos
            ++ [ borderOutset
               , borderWidth 4
               , backgroundColor (whitePct 80)
               ]
        )
        []


stringTile pos string xs =
    div (commonAttrs pos ++ [ itemsCenter ] ++ xs) [ text string ]


commonAttrs pos =
    let
        sp =
            toScreenCords pos
    in
    [ styleWidth cellWidth
    , styleHeight cellWidth
    , absolute
    , transforms [ translate sp ]
    , overflowHidden
    , style "box-sizing" "border-box"

    --, style "outline" "1px solid dodgerblue"
    , stylePx "font-size" cellWidth
    , style "font-family" "monospace"
    , displayFlex
    , itemsCenter
    , justifyCenter
    , noSelection
    , color (whitePct 20)
    , onClick (Click pos)
    , E.preventDefaultOn "contextmenu" (JD.succeed ( RightClick pos, True ))
    ]



-- Style Helpers
--noinspection ElmUnusedSymbol


backgroundColorTransparent =
    backgroundColor "transparent"


bold =
    style "font-weight" "bold"


backgroundColor =
    style "background-color"


color =
    style "color"


transforms xs =
    style "transform" (String.join " " xs)


stylePx s n =
    style s (floatPx n)


floatPx n =
    String.fromFloat n ++ "px"


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


whitePct pct =
    "hsl(0,0%," ++ String.fromInt pct ++ "%)"


displayFlex =
    style "display" "flex"



--noinspection ElmUnusedSymbol


itemsStretch =
    alignItems "stretch"


alignItems =
    style "align-items"


justifyCenter =
    justifyContent "center"


justifyContent =
    style "justify-content"


noSelection =
    style "user-select" "none"


overflowHidden =
    style "overflow" "hidden"


itemsCenter =
    alignItems "center"


borderColor =
    style "border-color"


borderSolid =
    borderStyle "solid"


borderOutset =
    borderStyle "outset"


borderStyle =
    style "border-style"


borderWidth =
    stylePx "border-width"


opacity o =
    style "opacity" (String.fromFloat o)
