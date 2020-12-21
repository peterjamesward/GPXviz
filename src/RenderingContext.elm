module RenderingContext exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import DisplayOptions exposing (DisplayOptions)
import Length
import NodesAndRoads exposing (DrawingNode, DrawingRoad, LocalCoords, ScalingInfo)
import ViewTypes exposing (ViewingMode)


type alias RenderingContext =
    { displayOptions : DisplayOptions
    , currentNode : Maybe DrawingNode
    , markedNode : Maybe DrawingNode
    , nodeBox : BoundingBox3d Length.Meters LocalCoords
    , viewingMode : ViewingMode
    , smoothedBend : List DrawingRoad
    , nudgedRoads : List DrawingRoad
    , nudgedRegionStart : Maybe Int
    , verticalNudge : Float
    , horizontalNudge : Float
    }
