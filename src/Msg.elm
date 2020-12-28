module Msg exposing (..)

import Accordion exposing (AccordionEntry)
import DisplayOptions exposing (CurtainStyle)
import File exposing (File)
import Json.Encode as E
import Time
import Utils exposing (Point)
import ViewTypes exposing (ViewingMode)
import Html.Events.Extra.Mouse as Mouse


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
    | SmoothGradient Float
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
    | StraightenStraight
    | SetHorizontalNudgeFactor  Float
    | SetVerticalNudgeFactor  Float
    | NudgeNode Float Float -- Horizontal, Vertical
    | SplitRoad
    | AccordionMessage (AccordionEntry Msg)
    | DeleteCurrentPoint Int
    | ChangeLoopStart Int
    | ReverseTrack
    | SetMaxTrackpointSpacing Float
    | MapMessage E.Value
    | MapRemoved String
    | MouseWheel Float
    | MouseClick Mouse.Event
    | MouseDoubleClick Mouse.Event
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | NoOpMsg
    | SimplifyTrack


type NodeSplitDirection
    = InsertNodeBefore
    | InsertNodeAfter
