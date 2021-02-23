module Msg exposing (..)

import Accordion exposing (AccordionEntry)
import DisplayOptions exposing (CurtainStyle)
import Element exposing (Element)
import File exposing (File)
import GeoCodeDecoders exposing (IpInfo, IpInfo)
import Graph
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Encode as E
import OAuthTypes exposing (OAuthMsg)
import StravaTypes exposing (StravaRoute, StravaSegment, StravaSegmentStreams)
import Time
import ViewTypes exposing (ViewingMode)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | GpxDownloaded (Result Http.Error String)
    | UserMovedNodeSlider Int
    | LocateProblem Int
    | PositionBackOne
    | PositionForwardOne
    | ChooseViewMode ViewingMode
    | ZoomLevelOverview Float
    | ZoomLevelFirstPerson Float
    | ZoomLevelThirdPerson Float
    | ZoomLevelPlan Float
    | ZoomLevelProfile Float
    | TogglePillars Bool
    | ToggleLighting Bool
    | ToggleSeaLevel Bool
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
    | SetVerticalExaggeration Float
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
    | DeleteTrackPoints (Int, Int)
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
    | UserChangedSegmentId String
    | LoadExternalSegment
    | UserChangedRouteId String
    | LoadExternalRoute
    | LoadSegmentStreams
    | HandleSegmentData (Result Http.Error StravaSegment)
    | HandleSegmentStreams (Result Http.Error StravaSegmentStreams)
    | HandleRouteData (Result Http.Error StravaRoute)
    | OAuthMessage OAuthMsg
    | ToggleMapNodesDraggable Bool
    | AutoFix (List Int)
    | UserChangedFilename String
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | FilterWeightedAverage
    | SetFilterBias Float
    | BezierSplines
    | SetBezierTension Float
    | SetBezierTolerance Float
    | GraphMsg Graph.Msg


wrapAuthMessage : OAuthMsg -> Msg
wrapAuthMessage msg =
    OAuthMessage msg

wrapGraphMessage : Graph.Msg -> Msg
wrapGraphMessage msg =
    GraphMsg msg


type NodeSplitDirection
    = InsertNodeBefore
    | InsertNodeAfter
