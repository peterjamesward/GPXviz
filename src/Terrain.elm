module Terrain exposing (makeTerrain)

import Array exposing (Array)
import Color
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d, Error, faces, fromVerticesBy)
import Dict
import Length exposing (Meters)
import LineSegment3d
import List
import NodesAndRoads exposing (DrawingRoad, MyCoord, deriveNodes, deriveRoads)
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


pointToComparable : Point3d Meters MyCoord -> ( ( Float, Float ), Point3d Meters MyCoord )
pointToComparable p3d =
    -- So we can use a Dict to de-dupe points in 2D.
    ( ( Length.inMeters <| Point3d.xCoordinate p3d
      , Length.inMeters <| Point3d.yCoordinate p3d
      )
    , p3d
    )


makeTerrain :
    RenderingContext
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeTerrain context roads =
    let
        roadList =
            Array.toList roads ++ theEdgesOfOurWorld

        roadVertices =
            List.concatMap entryEdges (List.take 1 roadList)
                ++ List.concatMap exitEdges roadList

        uniqueVertices =
            roadVertices
                |> List.map pointToComparable
                |> Dict.fromList
                |> Dict.values

        maybeTriangulation =
            -- This looks like the perfect constructor for us, if
            -- we ignore that we ignore the last trackpoint. (Experiment, right?)
            fromVerticesBy
                (Point3d.projectInto SketchPlane3d.xy)
            <|
                Array.fromList uniqueVertices

        --roadVertices
        corner lat lon ele =
            -- Make a trackpoint to locate each of our corners of the world.
            { lat = lat
            , lon = lon
            , ele = ele
            , idx = 0
            }

        entryEdges road =
            -- Totally forget why these are here but they seem to be essential.
            List.take 2 <| roadCorners road

        exitEdges road =
            List.drop 2 <| roadCorners road

        roadCorners road =
            -- Duplicated from VisualEntities with no apology.
            let
                kerbX =
                    -- Road is assumed to be 6 m wide.
                    3.0 * cos road.bearing

                kerbY =
                    3.0 * sin road.bearing

                depressLand =
                    0.5

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    ( ( road.startsAt.x, road.startsAt.y, road.startsAt.z - depressLand )
                    , ( road.endsAt.x, road.endsAt.y, road.endsAt.z - depressLand )
                    )

                roadAsSegment =
                    LineSegment3d.from
                        (Point3d.meters x1 y1 z1)
                        (Point3d.meters x2 y2 z2)

                perpendicular =
                    LineSegment3d.perpendicularDirection roadAsSegment

                leftKerb =
                    LineSegment3d.from
                        (Point3d.meters (x1 - kerbX) (y1 + kerbY) z1)
                        (Point3d.meters (x2 - kerbX) (y2 + kerbY) z2)

                leftHalfway =
                    LineSegment3d.from
                        (Point3d.meters (x1 - 0.5 * kerbX) (y1 + 0.5 * kerbY) z1)
                        (Point3d.meters (x2 - 0.5 * kerbX) (y2 + 0.5 * kerbY) z2)

                rightKerb =
                    LineSegment3d.from
                        (Point3d.meters (x1 + kerbX) (y1 - kerbY) z1)
                        (Point3d.meters (x2 + kerbX) (y2 - kerbY) z2)

                rightHalfway =
                    LineSegment3d.from
                        (Point3d.meters (x1 + 0.5 * kerbX) (y1 - 0.5 * kerbY) z1)
                        (Point3d.meters (x2 + 0.5 * kerbX) (y2 - 0.5 * kerbY) z2)
            in
            [ LineSegment3d.interpolate roadAsSegment 0.1
            , LineSegment3d.interpolate roadAsSegment 0.9
            , LineSegment3d.interpolate leftKerb 0.0
            , LineSegment3d.interpolate leftKerb 1.0
            , LineSegment3d.interpolate leftHalfway 0.1
            , LineSegment3d.interpolate leftHalfway 0.9
            , LineSegment3d.interpolate rightHalfway 0.1
            , LineSegment3d.interpolate rightHalfway 0.9
            , LineSegment3d.interpolate rightKerb 0.0
            , LineSegment3d.interpolate rightKerb 1.0
            ]

        borderLand =
            1000.0 / metresPerDegreeLatitude

        pointsBetween maxPoint ( ax, ay, az ) ( bx, by, bz ) =
            List.range 0 (maxPoint - 1)
                |> List.map (\i -> toFloat i / toFloat maxPoint)
                |> List.map
                    (\x ->
                        corner (ax * x + bx * (1.0 - x))
                            (ay * x + by * (1.0 - x))
                            (az * x + bz * (1.0 - x))
                    )

        sw =
            ( context.scaling.mins.lat - borderLand
            , context.scaling.mins.lon - borderLand
            , context.scaling.mins.ele
            )

        nw =
            ( context.scaling.maxs.lat + borderLand
            , context.scaling.mins.lon - borderLand
            , context.scaling.mins.ele
            )

        ne =
            ( context.scaling.maxs.lat + borderLand
            , context.scaling.maxs.lon + borderLand
            , context.scaling.mins.ele
            )

        se =
            ( context.scaling.mins.lat - borderLand
            , context.scaling.maxs.lon + borderLand
            , context.scaling.mins.ele
            )

        externalTrackpoints =
            -- Not pretty, but heh.
            -- NB we return to the first corner because we're only using road starts!
            -- Let's try putting some along the edges, not only corners.
            -- This seems to help, but not too many. (?)
            []
                ++ pointsBetween 1 sw nw
                ++ pointsBetween 1 nw ne
                ++ pointsBetween 1 ne se
                ++ pointsBetween 1 se sw

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
                    Scene3d.facet
                        (Material.matte <| Color.darkGreen)
                    <|
                        Triangle3d.from v1 v2 v3
            in
            List.map drawFace (faces triangulation)
