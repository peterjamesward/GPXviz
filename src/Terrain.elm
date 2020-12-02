module Terrain exposing (makeTerrain)

import Array exposing (Array)
import BoundingBox2d exposing (expandBy, fromExtrema, hullOfN)
import Color
import Dict exposing (Dict)
import Length exposing (Meters)
import List exposing (foldl)
import NodesAndRoads exposing (DrawingNode, DrawingRoad, GroundCoords, deriveNodes, deriveRoads)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Triangle3d
import VoronoiDiagram2d exposing (fromVerticesBy, polygons)


type alias PlanarLocation =
    Point2d Meters GroundCoords


type alias SpatialLocation =
    Point3d Meters GroundCoords



{-
   Experiment with VoronoiDiagram2d.

   1.  Collection of nodes (as now).
   1a. Possibly, remove any duplicates in 2d. (Dict.) (Might leave orphan 3d points.)
   2.  Voronoi.
   3.  Take list (vertex + list vPoints) and invert into list (vPoint + list vertx)
   4.  For each vPoint, form Polygon2d from the list vertex (maybe use convexHull constructor).
   5.  Convert back to 3d, giving vPoint mean height of vertices.
   6.  Triangles from vPoint to each polygon each.
   That's the first experiment. If that works then:
   7.  For each vertex in each polygon, move to the kerb nearest the vPoint.
   8.  Then draw the triangles.
-}


polygonFlip :
    List ( DrawingNode, Polygon2d Meters GroundCoords )
    -> List ( PlanarLocation, List DrawingNode )
polygonFlip polygons =
    -- Voronoi polygons are centred on our Nodes and list the polygon's vertices.
    -- We want to draw triangle-filled polygons that are centred on these vertices
    -- and bordered by our Nodes.
    -- Should be able to do this by running through the given list and
    -- accumulating vertices in a Dict. We'll see.
    let
        invertDict =
            List.foldl invertPolygon Dict.empty polygons

        verticesAsTuples polygon =
            List.map
                (Point2d.toTuple Length.inMeters)
                (Polygon2d.vertices polygon)

        invertPolygon ( node, polygon ) dict =
            List.foldl
                (addVertexToDict node)
                dict
                (verticesAsTuples polygon)

        addVertexToDict node vertex dict =
            Dict.update
                vertex
                (\v -> Just <| node :: Maybe.withDefault [] v)
                dict

        comparableToPoint ( tuple, list ) =
            ( Point2d.fromTuple Length.meters tuple, list )
    in
    Dict.toList invertDict
        |> List.map comparableToPoint


makeTerrain :
    RenderingContext
    -> Array DrawingRoad
    -> Array DrawingNode
    -> List (Entity GroundCoords)
makeTerrain context roads nodes =
    let
        pointFromNode : DrawingNode -> PlanarLocation
        pointFromNode node =
            Point2d.meters node.x node.y

        maybeBoundingBox =
            hullOfN pointFromNode (Array.toList nodes)

        maybeVoronoi =
            VoronoiDiagram2d.fromVerticesBy
                pointFromNode
                nodes

        meanHeight noods =
            (List.sum <| List.map .z noods) / (toFloat <| List.length noods)

        drawPolygon :
            ( PlanarLocation, List DrawingNode )
            -> List (Entity GroundCoords)
        drawPolygon ( centre, vertices ) =
            -- Need the central point's height to be mean of nodes', say.
            let
                centralPoint =
                    Point3d.xyz
                        (Point2d.xCoordinate centre)
                        (Point2d.yCoordinate centre)
                        (meanHeight vertices |> Length.meters)

                drawTriangle :
                    DrawingNode
                    -> DrawingNode
                    -> Entity GroundCoords
                drawTriangle v1 v2 =
                    Scene3d.facet
                        (Material.matte <| Color.darkGreen)
                        <| Triangle3d.from
                            centralPoint
                            (Point3d.meters v1.x v1.y v1.z)
                            (Point3d.meters v2.x v2.y v2.z)
            in
            List.map2
                drawTriangle
                vertices
                (List.drop 1 vertices
                    ++ vertices
                    ++ List.take 1 vertices
                )
    in
    case ( maybeBoundingBox, maybeVoronoi ) of
        ( Just boundingBox, Ok voronoi ) ->
            -- Cool, now we can get the Faces out, and with some luck, make up
            -- some triangles with the right height info.
            let
                stretchedBoundingBox =
                    -- Our Voronoi clipping area extends beyond the roads.
                    expandBy
                        (Length.meters <| 100.0 * context.scaling.metresToClipSpace)
                        boundingBox

                polygons =
                    VoronoiDiagram2d.polygons
                        stretchedBoundingBox
                        voronoi

                flippedPolygons =
                    -- Gives us polygons in which DrawingNodes are the vertices.
                    -- We should be able to make and draw 3d triangles from them.
                    polygonFlip polygons
            in
            List.concatMap drawPolygon flippedPolygons

        _ ->
            []



-- An error symbol of some kind ??
