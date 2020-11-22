module ScalingInfo exposing (..)

import TrackPoint exposing (TrackPoint)


type alias ScalingInfo =
    { mins : TrackPoint
    , maxs : TrackPoint
    , centres : TrackPoint
    , largestDimension : Float -- biggest bounding box edge determines scaling factor
    , seaLevelInClipSpace : Float
    , metresToClipSpace : Float -- Probably should be a proper metric tag!
    }
