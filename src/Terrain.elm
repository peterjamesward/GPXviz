module Terrain exposing (makeTerrain)

import Array exposing (Array)
import BoundingBox2d exposing (expandBy, fromExtrema, hullOfN)
import Color
import Length exposing (Meters)
import List
import NodesAndRoads exposing (DrawingNode, DrawingRoad, GroundCoords, deriveNodes, deriveRoads)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Triangle3d
import VoronoiDiagram2d exposing (fromVerticesBy, polygons)

type alias PlanarLocation = Point2d Meters GroundCoords

type alias SpatialLocation = Point3d Meters GroundCoords

{-
    Experiment with VoronoiDiagram2d.

    1.  Collection of nodes (as now).
    1a. Possibly, remove any duplicates in 2d. (Might leav orphan 3d points.)
    2.  Voronoi.
    3.  Take list (vertex + list vPoints) and invert into list (vPoint + list vertx)
    4.  For each vPoint, form Polygon2d from the list vertex (maybe use convexHull constructor).
    5.  Convert back to 3d, giving vPoint mean height of vertices.
    6.  Triangles from vPoint to each polygon each.
    That's the first experiment. If that works then:
    7.  For each vertex in each polygon, move to the kerb nearest the vPoint.
    8.  Then draw the triangles.
-}

{-
    Is there a much easier of doing this just by using the road edges to defin
    polygons, which geometry2d can triangulate? Would need to detect loops.
    Shall we try this with Hillingdon & Whiteleaf, and non-loop one like Dunstable?
-}

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

        maybeVoronoi =
            -- This looks like the perfect constructor for us, if
            -- we ignore that we ignore the last trackpoint. (Experiment, right?)
            VoronoiDiagram2d.fromVerticesBy
                pointFromNode
                nodes

        maybeBoundingBox =
            hullOfN pointFromNode (Array.toList nodes)

        invert : List (DrawingNode, PlanarLocation) -> List (PlanarLocation, List DrawingNode)
        invert voronoiPolygons =

        applyHeight : List (PlanarLocation, List DrawingNode) -> List (SpatialLocation, List DrawingNode)
        applyHeight inversion =

    in
    case (maybeVoronoi, maybeBoundingBox) of
        (Ok voronoi, Just boundingBox)  ->
            -- Cool, now we can get the Faces out, and with some luck, make up
            -- some triangles with the right height info.
            let
                stretchedBoundingBox =
                    expandBy (Length.meters 100.0) boundingBox

                voronoiPolygons = polygons stretchedBoundingBox voronoi

                drawFace face =
                    Scene3d.facet
                        (Material.matte <| Color.darkGreen)
                    <|
                        Triangle3d.fromVertices face.vertices
            in
            List.map drawFace (faces triangulation)

        _ ->
            [] -- An error symbol of some kind ??
