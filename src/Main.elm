module Main exposing (main)

import Board exposing (Board)
import Browser
import Dict
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events as E exposing (onClick)
import IntSize as Size exposing (IntSize)
import Json.Decode as JD
import Lid exposing (Lid)
import MineCell as Mine
import Random exposing (Seed)


main =
    Browser.element
        { init = init
        , view = view
        , update = \msg -> update msg >> (\mo -> ( mo, Cmd.none ))
        , subscriptions = always Sub.none
        }


type alias Model =
    { board : Board
    , gameState : GameState
    , seed : Seed
    }


type alias GameState =
    Board.State


type alias Pos =
    ( Int, Int )


init : { now : Int } -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            flags.now
                |> always 2
                |> Random.initialSeed
    in
    ( generateModel initialSeed
        |> update (Click ( 0, 0 ))
        |> update (RightClick ( 4, 0 ))
      --|> update (Click ( 4, 1 ))
    , Cmd.none
    )


generateModel : Seed -> Model
generateModel initialSeed =
    let
        generator =
            Board.generator gridSize

        ( board, seed ) =
            Random.step generator initialSeed
    in
    { board = board
    , gameState = Board.PlayerTurn
    , seed = seed
    }


reset : Model -> Model
reset model =
    generateModel model.seed


type Msg
    = Click Pos
    | RightClick Pos
    | ResetClicked


update : Msg -> Model -> Model
update msg model =
    case ( model.gameState, msg ) of
        ( Board.PlayerTurn, Click pos ) ->
            case Board.openLidAt pos model.board of
                Just ( gameState, board ) ->
                    { model
                        | gameState = gameState
                        , board = board
                    }

                Nothing ->
                    model

        ( Board.PlayerTurn, RightClick loc ) ->
            case Board.cycleLabel loc model.board of
                Just board ->
                    { model | board = board }

                Nothing ->
                    model

        ( Board.Lost, Click _ ) ->
            reset model

        ( _, ResetClicked ) ->
            reset model

        _ ->
            model


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
            Board.PlayerTurn ->
                model.board
                    |> Board.toDict
                    |> Dict.toList
                    |> List.concatMap
                        (\( pos, ( lid, cell ) ) ->
                            renderTileView pos (toPlayerTurnTileView lid cell)
                        )

            Board.Lost ->
                model.board
                    |> Board.toDict
                    |> Dict.toList
                    |> List.concatMap
                        (\( pos, ( lid, cell ) ) ->
                            renderTileView pos (toLostTileView lid cell)
                        )
        )


toPlayerTurnTileView lid cell =
    case lid of
        Lid.Open ->
            case cell of
                Mine.Mine ->
                    MineView

                Mine.Empty 0 ->
                    EmptyView

                Mine.Empty n ->
                    NumView n

        Lid.Closed ->
            ClosedView

        Lid.Flagged ->
            FlagView


toLostTileView lid cell =
    case cell of
        Mine.Mine ->
            case lid of
                Lid.Open ->
                    ExplodingMineView

                Lid.Closed ->
                    MineView

                Lid.Flagged ->
                    SuccessMineView

        Mine.Empty 0 ->
            case lid of
                Lid.Open ->
                    EmptyView

                Lid.Closed ->
                    EmptyView

                Lid.Flagged ->
                    FailureEmptyView

        Mine.Empty n ->
            case lid of
                Lid.Open ->
                    NumView n

                Lid.Closed ->
                    NumView n

                Lid.Flagged ->
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
            [ baseTile pos [], mineTile pos ]

        ExplodingMineView ->
            [ baseTile pos [ backgroundColor "hsla(0, 90%, 75%, 1)" ], mineTile pos ]

        SuccessMineView ->
            [ baseTile pos [], mineTile pos, tickTile pos ]

        FailureEmptyView ->
            [ baseTile pos [], crossTile pos ]

        FailureNumView n ->
            [ baseTile pos [], numTile pos n, crossTile pos ]

        EmptyView ->
            [ baseTile pos [] ]

        NumView n ->
            [ baseTile pos [], numTile pos n ]

        ClosedView ->
            [ coverTile pos ]

        FlagView ->
            [ coverTile pos, flagTile pos ]


numTile pos n =
    stringTile pos (String.fromInt n) []


crossTile pos =
    stringTile pos "❌" [ color "red", bold, opacity 0.6 ]


tickTile pos =
    stringTile pos "✔" [ color "green" ]


mineTile pos =
    stringTile pos
        "💣"
        [ opacity 0.7, fontSize (cellWidth * 0.7) ]


flagTile pos =
    stringTile pos "⛳" []


baseTileStyles =
    [ borderSolid
    , borderWidth 0.5
    , borderColor (whitePct 80)
    , backgroundColor (whitePct 90)
    ]


baseTile pos xs =
    div
        (commonAttrs pos
            ++ baseTileStyles
            ++ xs
        )
        []


coverTile pos =
    div
        (commonAttrs pos
            ++ [ borderOutset
               , borderWidth 4
               , backgroundColor (whitePct 80)
               ]
        )
        []


stringTile pos string xs =
    div (commonAttrs pos ++ xs) [ text string ]


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
    , borderBox
    , fontSize cellWidth
    , monospace
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


borderBox =
    style "box-sizing" "border-box"


monospace =
    style "font-family" "monospace"


fontSize =
    stylePx "font-size"
