module NodesAndRoads exposing (..)

import TrackPoint exposing (TrackPoint)


type MyCoord
    = SomeCoord


type alias DrawingNode =
    -- We will draw in a rectangular space using metre units. Probably.
    { trackPoint : TrackPoint
    , northOffset : Float -- metres from bottom edge of bounding box
    , eastOffset : Float -- metres from left edge of bounding box
    , vertOffset : Float -- metres from base of bounding box
    , x : Float -- east offset convrted to [-1, +1] system
    , y : Float -- north, ditto
    , z : Float -- vert, ditto
    }


type alias DrawingRoad =
    { startsAt : DrawingNode
    , endsAt : DrawingNode
    , length : Float
    , bearing : Float
    , gradient : Float -- radians
    , startDistance : Float
    , endDistance : Float
    , index : Int
    }
