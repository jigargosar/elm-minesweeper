module LidGrid exposing (Lid(..), LidGrid)

import IntSize exposing (IntSize)
import PosDict exposing (PosDict)


type Lid
    = Open
    | Closed
    | Flagged


type LidGrid
    = LidGrid IntSize (PosDict Lid)
