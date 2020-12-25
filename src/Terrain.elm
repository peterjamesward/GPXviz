module Terrain exposing (makeTerrain)

import Array exposing (Array)
import BoundingBox3d
import Color
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d, Error, faces, fromVerticesBy)
import Dict
import Length exposing (Meters)
import LineSegment3d
import List
import NodesAndRoads exposing (DrawingRoad, LocalCoords, deriveNodes, deriveRoads)
import Point3d exposing (Point3d)
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Triangle3d
import Vector3d



{-
   The idea here is to make a mesh using Delaunay Triangulation,
   included in the elm-geometry package.
   There are two complications remaining:
   1.  We shall need to work from the road edges rather than centres, so
       we will need the road segment corner coordinates. Ideally we would
       compute these once.
   2.  Probably needs some variation in colour.
-}


pointToComparable : Point3d Meters LocalCoords -> ( ( Float, Float ), Point3d Meters LocalCoords )
pointToComparable p3d =
    -- So we can use a Dict to de-dupe points in 2D.
    ( ( Length.inMeters <| Point3d.xCoordinate p3d
      , Length.inMeters <| Point3d.yCoordinate p3d
      )
    , p3d
    )


makeTerrain :
    RenderingContext
    -> List DrawingRoad
    -> List (Entity LocalCoords)
makeTerrain context roadList =
    let
        roadVertices =
            List.concatMap roadCorners (List.take 1 roadList)
                ++ List.concatMap roadCorners roadList

        uniqueVertices =
            (roadVertices ++ externalVertices)
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

        entryEdges road =
            roadCorners road

        exitEdges road =
            roadCorners road

        roadCorners road =
            -- Duplicated from VisualEntities with no apology.
            let
                ( kerbX, kerbY ) =
                    -- Road is assumed to be 6 m wide.
                    ( 3.0 * cos road.bearing
                    , 3.0 * sin road.bearing
                    )

                roadAsSegment =
                    -- Put the terrain underneath the road, by a foot or so.
                    LineSegment3d.translateBy
                        (Vector3d.meters 0.0 0.0 -0.3)
                    <|
                        LineSegment3d.fromEndpoints ( road.startsAt.location, road.endsAt.location )

                leftKerbVector =
                    Vector3d.meters
                        (-1.0 * kerbX)
                        kerbY
                        0.0

                rightKerbVector =
                    Vector3d.reverse leftKerbVector

                ( leftKerb, rightKerb ) =
                    ( LineSegment3d.translateBy leftKerbVector roadAsSegment
                    , LineSegment3d.translateBy rightKerbVector roadAsSegment
                    )

                ( leftHalfwayVector, rightHalfwayVector ) =
                    ( Vector3d.half leftKerbVector
                    , Vector3d.half rightKerbVector
                    )

                ( leftHalfway, rightHalfway ) =
                    ( LineSegment3d.translateBy leftHalfwayVector roadAsSegment
                    , LineSegment3d.translateBy rightHalfwayVector roadAsSegment
                    )
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

        expandedBox =
            -- Tedious bit where we establish our periphery.
            BoundingBox3d.expandBy (Length.meters 1000.0) context.nodeBox

        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema expandedBox

        { midX, midY, midZ } =
            { midX = BoundingBox3d.midX expandedBox
            , midY = BoundingBox3d.midY expandedBox
            , midZ = BoundingBox3d.midZ expandedBox
            }

        { n, s, e, w } =
            { n = Point3d.xyz midX maxY midZ
            , s = Point3d.xyz midX minY midZ
            , e = Point3d.xyz maxY midY midZ
            , w = Point3d.xyz minX midY midZ
            }

        { sw, nw, ne, se } =
            { sw = Point3d.xyz minX minY midZ
            , nw = Point3d.xyz minX maxY midZ
            , ne = Point3d.xyz maxX maxY midZ
            , se = Point3d.xyz maxX minY midZ
            }

        externalVertices =
            [ n, e, s, w, ne, nw, se, sw ]
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
                            -- Note we're using the original 3D vertices here!
                            face.vertices
                    in
                    Scene3d.facet
                        (Material.matte <| Color.darkGreen)
                    <|
                        Triangle3d.from v1 v2 v3
            in
            List.map drawFace (faces triangulation)
