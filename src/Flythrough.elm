module Flythrough exposing (..)

import NodesAndRoads exposing (DrawingRoad)
import Time


type alias Flythrough =
    { cameraPosition : ( Float, Float, Float )
    , focusPoint : ( Float, Float, Float )
    , metresFromRouteStart : Float
    , lastUpdated : Time.Posix
    , running : Bool
    , segment : DrawingRoad
    }
