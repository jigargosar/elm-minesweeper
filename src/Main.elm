module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events as E exposing (onClick)
import Json.Decode as JD
import Random
import Set exposing (Set)
import String exposing (fromFloat, fromInt)
import Tuple exposing (first, pair, second)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { open : Set Loc
    , ts : Dict Loc TileState
    }


init : Model
init =
    let
        testOpenedLoc =
            collectZeroNeighbours ( 0, 0 ) Set.empty Set.empty
                |> Debug.log "debug"

        openTestLoc ts =
            Set.foldl (\l -> Dict.insert l Open) ts testOpenedLoc
    in
    { open = Set.empty
    , ts =
        gridPS
            |> List.map (\loc -> ( loc, Closed ))
            |> Dict.fromList
            |> openTestLoc
    }


type Msg
    = Click Loc
    | RightClick Loc


type alias Loc =
    ( Int, Int )


collectZeroNeighbours loc pending collected =
    let
        nCollected =
            Set.insert loc collected
    in
    case
        Set.diff (neighboursHavingZeroNeighbouringMines loc) collected
            |> Set.union pending
            |> Set.toList
    of
        [] ->
            nCollected
                |> Set.foldl (\z -> Set.union (z |> neighbourLocations |> List.filter isValidLoc |> Set.fromList)) nCollected

        x :: xs ->
            collectZeroNeighbours x (Set.fromList xs) nCollected


neighboursHavingZeroNeighbouringMines loc =
    loc
        |> validNeighbours
        |> Set.filter (\n -> neighbourMineCount n == 0)


validNeighbours loc =
    loc
        |> neighbourLocations
        |> Set.fromList
        |> Set.filter isValidLoc


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
                            { model | ts = Dict.insert loc Open model.ts }

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
                            { model | ts = Dict.insert loc Flagged model.ts }

                        Flagged ->
                            { model | ts = Dict.insert loc Closed model.ts }

                Nothing ->
                    model


isValidLoc : Loc -> Bool
isValidLoc =
    isInvalidLoc >> not


isInvalidLoc : Loc -> Bool
isInvalidLoc ( x, y ) =
    x < 0 || y < 0 || x >= gridWidth || y >= gridHeight


tsAt : Model -> Loc -> Maybe TileState
tsAt model loc =
    Dict.get loc model.ts


type TileState
    = Open
    | Closed
    | Flagged


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
viewTile m loc =
    let
        sp =
            toScreenCords loc
    in
    div
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
        [ case tsAt m loc of
            Just Open ->
                text (tileAt loc)

            Just Flagged ->
                text "F"

            _ ->
                text ""
        ]


tileAt loc =
    if isMine loc then
        "*"

    else
        let
            mc =
                neighbourMineCount loc
        in
        if mc > 0 then
            fromInt mc

        else
            --""
            fromInt mc


neighbourMineCount loc =
    neighbourLocations loc
        |> List.filter isMine
        |> List.length


isMine loc =
    List.member loc mines


neighbourLocations ( x, y ) =
    List.map (\( dx, dy ) -> ( x + dx, y + dy )) unitNeighbours


unitNeighbours =
    [ [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
    , [ ( -1, 0 ), ( 1, 0 ) ]
    , [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
    ]
        |> List.concat



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
