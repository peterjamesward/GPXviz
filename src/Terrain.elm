module Terrain exposing (makeTerrain)

import Array exposing (Array)
import NodesAndRoads exposing (DrawingRoad, MyCoord)
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)


makeTerrain :
    RenderingContext
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeTerrain context roads =
    []
