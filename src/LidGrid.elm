module LidGrid exposing (Lid(..), LidGrid, cycleLabelIfNotOpen, fillClosed, get, open, openIfClosed)

import Dict
import IntSize exposing (IntSize)
import PosDict exposing (PosDict)


type Lid
    = Open
    | Closed
    | Flagged


type LidGrid
    = LidGrid IntSize (PosDict Lid)


fillClosed : IntSize -> LidGrid
fillClosed size =
    PosDict.init size (always Closed)
        |> LidGrid size


setAt : ( Int, Int ) -> Lid -> LidGrid -> LidGrid
setAt pos lid =
    mapAt pos (always lid)


mapAt : ( Int, Int ) -> (Lid -> Lid) -> LidGrid -> LidGrid
mapAt pos f (LidGrid s d) =
    LidGrid s (Dict.update pos (Maybe.map f) d)


get : LidGrid -> ( Int, Int ) -> Maybe Lid
get (LidGrid _ d) pos =
    Dict.get pos d


open : ( Int, Int ) -> LidGrid -> LidGrid
open pos =
    setAt pos Open


openIfClosed : ( Int, Int ) -> LidGrid -> LidGrid
openIfClosed pos =
    mapAt pos
        (\lid ->
            if lid == Closed then
                Open

            else
                lid
        )


cycleLabelIfNotOpen : ( Int, Int ) -> LidGrid -> LidGrid
cycleLabelIfNotOpen pos =
    mapAt pos
        (\lid ->
            case lid of
                Open ->
                    lid

                Closed ->
                    Flagged

                Flagged ->
                    Closed
        )
