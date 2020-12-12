module Msg exposing (..)

import Accordion exposing (AccordionEntry)
import DisplayOptions exposing (CurtainStyle)
import File exposing (File)
import Time
import Utils exposing (Point)
import ViewTypes exposing (ViewingMode)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | UserMovedNodeSlider Int
    | PositionBackOne
    | PositionForwardOne
    | ChooseViewMode ViewingMode
    | ZoomLevelOverview Float
    | ZoomLevelFirstPerson Float
    | ZoomLevelThirdPerson Float
    | ZoomLevelPlan Float
    | ZoomLevelProfile Float
    | ImageGrab Point
    | ImageRotate Point
    | ImageRelease Point
    | TogglePillars Bool
    | ToggleRoad Bool
    | ToggleCones Bool
    | ToggleCentreLine Bool
    | SetCurtainStyle CurtainStyle
    | SetGradientChangeThreshold Float
    | SetBearingChangeThreshold Float
    | DeleteZeroLengthSegments
    | OutputGPX
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SetSmoothingEnd Int
    | SmoothGradient Int Int Float
    | SmoothBend
    | Undo
    | Redo
    | ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne
    | SetMaxTurnPerSegment Int
    | SetBumpinessFactor Float
    | SetFlythroughSpeed Float
    | RunFlythrough Bool
    | ResetFlythrough
    | InsertBeforeOrAfter Int NodeSplitDirection
    | MakeTerrain
    | ClearTerrain
    | CloseTheLoop
    | StraightenStraight Int Int
    | SetHorizontalNudgeFactor Int Float
    | SetVerticalNudgeFactor Int Float
    | NudgeNode Int Float Float -- Horizontal, Vertical
    | SplitRoad Int
    | AccordionMessage (AccordionEntry Msg)
    | DeleteCurrentPoint Int
    | ChangeLoopStart Int


type NodeSplitDirection
    = InsertNodeBefore
    | InsertNodeAfter
