module RenderingContext exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import DisplayOptions exposing (DisplayOptions)
import Graph exposing (Graph)
import Length
import NodesAndRoads exposing (DrawingNode, DrawingRoad, ScalingInfo)
import Point3d exposing (Point3d)
import UbiquitousTypes exposing (LocalCoords)
import ViewTypes exposing (ViewingMode)


type alias RenderingContext =
    { displayOptions : DisplayOptions
    , currentNode : Maybe DrawingNode
    , markedNode : Maybe DrawingNode
    , nodeBox : BoundingBox3d Length.Meters LocalCoords
    , viewingMode : ViewingMode
    , smoothedBend : List (Point3d Length.Meters LocalCoords)
    , nudgedRoads : List DrawingRoad
    , nudgedRegionStart : Maybe Int
    , verticalNudge : Float
    , horizontalNudge : Float
    , zoomLevel : Float
    , verticalExaggeration : Float
    , graph : Graph
    }
