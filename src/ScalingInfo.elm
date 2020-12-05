module ScalingInfo exposing (..)

import TrackPoint exposing (TrackPoint)


type alias ScalingInfo =
    { mins : TrackPoint
    , maxs : TrackPoint
    , centres : TrackPoint
    }
