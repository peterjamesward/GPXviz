module RenderingContext exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import DisplayOptions exposing (DisplayOptions)
import Length
import NodesAndRoads exposing (DrawingRoad, ScalingInfo)
import Point3d exposing (Point3d)
import TrackPoint exposing (TrackPoint)
import UbiquitousTypes exposing (LocalCoords)
import ViewTypes exposing (ViewingMode)


type alias RenderingContext =
    { displayOptions : DisplayOptions
    , currentNode : Maybe TrackPoint
    , markedNode : Maybe TrackPoint
    , nodeBox : BoundingBox3d Length.Meters LocalCoords
    , viewingMode : ViewingMode
    , smoothedBend : List (Point3d Length.Meters LocalCoords)
    , nudgedRoads : List DrawingRoad
    , nudgedRegionStart : Maybe Int
    , verticalNudge : Float
    , horizontalNudge : Float
    , zoomLevel : Float
    , verticalExaggeration : Float
    , graphNodes : List TrackPoint
    }
