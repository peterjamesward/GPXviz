module Msg exposing (..)

import Accordion exposing (AccordionEntry)
import DisplayOptions exposing (CurtainStyle)
import File exposing (File)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Encode as E
import OAuthTypes exposing (OAuthMsg)
import StravaSegment exposing (StravaSegment)
import Time
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
    | SetBendTrackPointSpacing Float
    | SetBumpinessFactor Float
    | SetFlythroughSpeed Float
    | RunFlythrough Bool
    | ResetFlythrough
    | InsertBeforeOrAfter Int NodeSplitDirection
    | MakeTerrain
    | ClearTerrain
    | CloseTheLoop
    | StraightenStraight
    | SetHorizontalNudgeFactor Float
    | SetVerticalNudgeFactor Float
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
    | Autosmooth (List Int)
    | UserChangedUrl String
    | LoadExternalSegment
    | HandleSegmentData (Result Http.Error StravaSegment)
    | OAuthMessage OAuthMsg

wrapAuthMessage : OAuthMsg -> Msg
wrapAuthMessage msg =
    OAuthMessage msg


type NodeSplitDirection
    = InsertNodeBefore
    | InsertNodeAfter
