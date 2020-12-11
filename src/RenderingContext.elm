module RenderingContext exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import DisplayOptions exposing (DisplayOptions)
import Length
import NodesAndRoads exposing (DrawingRoad, LocalCoords, ScalingInfo)
import ViewTypes exposing (ViewingMode)


type alias RenderingContext =
    { displayOptions : DisplayOptions
    , currentNode : Maybe DrawingRoad
    , markedNode : Maybe DrawingRoad
    , nodeBox : BoundingBox3d Length.Meters LocalCoords
    , viewingMode : ViewingMode
    , smoothedBend : List DrawingRoad
    , verticalNudge : Float
    , horizontalNudge : Float
    }
