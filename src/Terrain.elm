module Terrain exposing (makeTerrain)

import Array exposing (Array)
import Color
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d, Error, faces, fromVerticesBy)
import Length exposing (Meters)
import List exposing (take)
import NodesAndRoads exposing (DrawingRoad, MyCoord, deriveNodes, deriveRoads)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Spherical exposing (metresPerDegreeLatitude)
import Triangle3d



{-
   The idea here is to make a mesh using Delaunay Triangulation,
   included in the elm-geometry package.
   There are two complications remaining:
   1.  We shall need to work from the road edges rather than centres, so
       we will need the road segment corner coordinates. Ideally we would
       compute these once.
   2.  Probably needs some variation in colour.
-}


makeTerrain :
    RenderingContext
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeTerrain context roads =
    let
        convertTo2d p3d =
            let
                ( x, y, _ ) =
                    Point3d.toTuple Length.inMeters p3d
            in
            Point2d.meters x y

        roadList =
            Array.toList roads ++ theEdgesOfOurWorld

        roadVertices =
            List.concatMap entryEdges (List.take 1 roadList)
                ++ List.concatMap exitEdges roadList

        maybeTriangulation =
            -- This looks like the perfect constructor for us, if
            -- we ignore that we ignore the last trackpoint. (Experiment, right?)
            fromVerticesBy
                (Point3d.projectInto SketchPlane3d.xy)
            <|
                Array.fromList roadVertices

        corner lat lon ele =
            -- Make a trackpoint to locate each of our corners of the world.
            { lat = lat
            , lon = lon
            , ele = ele
            , idx = 0
            }

        entryEdges road =
            List.take 2 <| roadCorners road

        exitEdges road =
            List.drop 2 <| roadCorners road

        roadCorners road =
            -- Duplicated from VisualEntities with no apology.
            let
                kerbX =
                    -- Road is assumed to be 6 m wide.
                    4.0 * cos road.bearing * context.scaling.metresToClipSpace

                kerbY =
                    4.0 * sin road.bearing * context.scaling.metresToClipSpace

                thickness =
                    0.6 * context.scaling.metresToClipSpace

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    ( ( road.startsAt.x, road.startsAt.y, road.startsAt.z )
                    , ( road.endsAt.x, road.endsAt.y, road.endsAt.z )
                    )
            in
            [ Point3d.meters (x1 + kerbX) (y1 - kerbY) (z1 - thickness)
            , Point3d.meters (x2 + kerbX) (y2 - kerbY) (z2 - thickness)
            , Point3d.meters (x2 - kerbX) (y2 + kerbY) (z2 - thickness)
            , Point3d.meters (x1 - kerbX) (y1 + kerbY) (z1 - thickness)
            ]

        boundary =
            1000.0 / metresPerDegreeLatitude

        externalTrackpoints =
            -- Not pretty, but heh.
            -- NB we return to the first corner because we're only using road starts!
            [ corner (context.scaling.mins.lat - boundary)
                (context.scaling.mins.lon - boundary)
                context.scaling.mins.ele
            , corner (context.scaling.mins.lat - boundary)
                (context.scaling.maxs.lon + boundary)
                context.scaling.mins.ele
            , corner (context.scaling.maxs.lat + boundary)
                (context.scaling.mins.lon - boundary)
                context.scaling.mins.ele
            , corner (context.scaling.maxs.lat + boundary)
                (context.scaling.maxs.lon + boundary)
                context.scaling.mins.ele
            , corner (context.scaling.mins.lat - boundary)
                (context.scaling.mins.lon - boundary)
                context.scaling.mins.ele
            ]

        theEdgesOfOurWorld =
            deriveNodes context.scaling externalTrackpoints
                |> deriveRoads
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
                    Scene3d.triangle (Material.color Color.darkGreen) <|
                        Triangle3d.from v1 v2 v3
            in
            List.map drawFace (faces triangulation)
