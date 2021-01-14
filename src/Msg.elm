module Msg exposing (..)

import Accordion exposing (AccordionEntry)
import DisplayOptions exposing (CurtainStyle)
import File exposing (File)
import GeoCodeDecoders exposing (IpInfo, IpInfo)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Encode as E
import KomootAuth
import StravaAuth as StravaAuth
import StravaTypes exposing (StravaRoute, StravaSegment, StravaSegmentStreams)
import Time
import ViewTypes exposing (ViewingMode)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | GpxDownloaded (Result Http.Error String)
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
    | ToggleLighting Bool
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
    | KomootAuthMessage KomootAuth.OAuthMsg
    | StravaAuthMessage StravaAuth.OAuthMsg
    | ToggleMapNodesDraggable Bool
    | AutoFix (List Int)
    | UserChangedFilename String
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | SendIpInfo
    | IpInfoAcknowledged (Result Http.Error ())
    | FilterWeightedAverage
    | ToggleFilterXY Bool
    | ToggleFilterZ Bool

wrapStravaAuthMessage : StravaAuth.OAuthMsg -> Msg
wrapStravaAuthMessage msg =
    StravaAuthMessage msg

wrapKomootAuthMessage : KomootAuth.OAuthMsg -> Msg
wrapKomootAuthMessage msg =
    KomootAuthMessage msg


type NodeSplitDirection
    = InsertNodeBefore
    | InsertNodeAfter
