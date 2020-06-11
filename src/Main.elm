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
    { tsDict =
        gridPS
            |> List.map (\loc -> ( loc, Closed ))
            |> Dict.fromList
    , gameState = PlayerTurn
    }


type Msg
    = Click Loc
    | RightClick Loc


collectZeroNeighbours loc =
    collectZeroNeighboursHelp loc Set.empty Set.empty


collectZeroNeighboursHelp loc pending collected =
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

        x :: xs ->
            collectZeroNeighboursHelp x (Set.fromList xs) nCollected


includeNeighboursOfEveryMember locSet =
    locSet
        |> Set.foldl (validNeighbours >> Set.union) locSet


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
                            if isMine loc then
                                { model | gameState = Lost }

                            else if neighbourMineCount loc == 0 then
                                let
                                    nts =
                                        collectZeroNeighbours loc
                                            |> includeNeighboursOfEveryMember
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

                            else
                                model

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


isValidLoc : Loc -> Bool
isValidLoc =
    isInvalidLoc >> not


isInvalidLoc : Loc -> Bool
isInvalidLoc ( x, y ) =
    x < 0 || y < 0 || x >= gridWidth || y >= gridHeight


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


mines : List ( Int, Int )
mines =
    let
        minePct =
            0.1

        minesGenerator =
            Random.list (List.length gridPS) (Random.weighted ( minePct, True ) [ ( 1 - minePct, False ) ])
                |> Random.map (\bs -> List.map2 pair gridPS bs |> List.filter second |> List.map first)
    in
    Random.step minesGenerator (Random.initialSeed 1)
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

        isOpenMine =
            tsAt m loc == Just Open && isMine loc
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
