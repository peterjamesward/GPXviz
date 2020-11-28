module Terrain exposing (makeTerrain)

import Array exposing (Array)
import Color
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d, Error, faces, fromVerticesBy)
import Length exposing (Meters)
import NodesAndRoads exposing (DrawingRoad, MyCoord)
import Point2d exposing (Point2d)
import Point3d
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import TrackPoint exposing (TrackPoint)
import Triangle3d



{-
   The idea here is to make a mesh using Delaunay Triangulation,
   included in the elm-geometry package.
   There are two complications:
   1.  We shall need to work from the road edges rather than centres, so
       we will need the road segment corner coordinates. Ideally we would
       compute these once.
   2.  The package implementation works in 2d, which is fine, but we will
       need to correlate the resulting triangles with our nodes so
       that we interpolate height.

   First step is to ignore (1) and do a quick (as possible) experiment.
   If the result is rubbish, no point pursuing.

   Quick assessment is that (2) is wrong, as we get our original vertex back (TBC).
-}


makeTerrain :
    RenderingContext
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeTerrain context roads =
    let
        pointFromRoad : DrawingRoad -> Point2d Meters MyCoord
        pointFromRoad road =
            Point2d.meters road.startsAt.x road.startsAt.y

        maybeTriangulation =
            -- This looks like the perfect constructor for us, if
            -- we ignore that we ignore the last trackpoint. (Experiment, right?)
            fromVerticesBy
                pointFromRoad
                roads
    in
    case maybeTriangulation of
        Err _ ->
            []

        Ok triangulation ->
            -- Cool, now we can get the Faces out, and with some luck, make up
            -- some triangles with the right height info.
            let
                drawFace face =
                    let
                        ( v1, v2, v3 ) =
                            -- Would be obviated by using geometry consistently!
                            face.vertices
                    in
                    Scene3d.triangle (Material.color Color.grey) <|
                        Triangle3d.from
                            (Point3d.meters v1.startsAt.x v1.startsAt.y v1.startsAt.z)
                            (Point3d.meters v2.startsAt.x v2.startsAt.y v2.startsAt.z)
                            (Point3d.meters v3.startsAt.x v3.startsAt.y v3.startsAt.z)
            in
            List.map drawFace (faces triangulation)
