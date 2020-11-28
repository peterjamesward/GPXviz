module Terrain exposing (makeTerrain)

import Array exposing (Array)
import Color
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d, Error, faces, fromVerticesBy)
import Length exposing (Meters)
import NodesAndRoads exposing (DrawingRoad, MyCoord, deriveNodes, deriveRoads)
import Point2d exposing (Point2d)
import Point3d
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Spherical exposing (metresPerDegreeLatitude)
import Triangle3d



{-
   The idea here is to make a mesh using Delaunay Triangulation,
   included in the elm-geometry package.
   There is one complications remaining:
   1.  We shall need to work from the road edges rather than centres, so
       we will need the road segment corner coordinates. Ideally we would
       compute these once.
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
                withCorners

        corner lat lon ele =
            -- Make a trackpoint to locate each of our corners.
            { lat = lat
            , lon = lon
            , ele = ele
            , idx = 0
            }

        boundary =
            1000.0 / metresPerDegreeLatitude

        cornerTrackpoints =
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

        cornerRoads =
            deriveNodes context.scaling cornerTrackpoints
                |> deriveRoads

        withCorners =
            Array.append roads <| Array.fromList cornerRoads
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
                        Triangle3d.from
                            (Point3d.meters v1.startsAt.x v1.startsAt.y v1.startsAt.z)
                            (Point3d.meters v2.startsAt.x v2.startsAt.y v2.startsAt.z)
                            (Point3d.meters v3.startsAt.x v3.startsAt.y v3.startsAt.z)
            in
            List.map drawFace (faces triangulation)
