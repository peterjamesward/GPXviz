module Filters exposing (..)

import TrackPoint exposing (TrackPoint)


applyWeightedAverageFilter : List TrackPoint -> List TrackPoint
applyWeightedAverageFilter points =
    points
