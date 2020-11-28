module RenderingContext exposing (..)

import DisplayOptions exposing (DisplayOptions)
import NodesAndRoads exposing (DrawingRoad)
import ScalingInfo exposing (ScalingInfo)
import ViewTypes exposing (ViewSubmode, ViewingMode)


type alias RenderingContext =
    { displayOptions : DisplayOptions
    , currentNode : Maybe DrawingRoad
    , markedNode : Maybe DrawingRoad
    , scaling : ScalingInfo
    , viewingMode : ViewingMode
    , viewingSubMode : ViewSubmode
    , smoothedBend : List DrawingRoad
    }
