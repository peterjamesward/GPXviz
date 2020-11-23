module Msg exposing (..)

import DisplayOptions exposing (CurtainStyle)
import File exposing (File)
import Time
import Utils exposing (Point)
import ViewTypes exposing (ThirdPersonSubmode, ViewingMode)


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
    | SetCurtainStyle CurtainStyle
    | SetGradientChangeThreshold Float
    | SetBearingChangeThreshold Float
    | DeleteZeroLengthSegments
    | OutputGPX
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SetSmoothingEnd Int
    | SmoothGradient Int Int Float
    | SetThirdPersonSubmode ThirdPersonSubmode
    | Undo
    | ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne
    | SetMaxTurnPerSegment Int
    | SetBumpinessFactor Float
    | SetFlythroughSpeed Float
    | RunFlythrough Bool
    | ResetFlythrough
    | VerticalNodeSpilt Int
