module VisualEntities exposing (..)

import Angle
import Array exposing (Array)
import Axis3d
import BoundingBox3d
import Circle3d
import Color exposing (blue)
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeZ, positiveZ, xComponent, yComponent)
import DisplayOptions exposing (CurtainStyle(..), DisplayOptions)
import Length exposing (meters)
import LineSegment3d
import NodesAndRoads exposing (DrawingRoad)
import Plane3d
import Point3d
import Quantity
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity, cone, cylinder, sphere, triangle)
import Scene3d.Material as Material
import Sphere3d exposing (Sphere3d)
import TrackPoint exposing (TrackPoint)
import Triangle3d exposing (Triangle3d)
import UbiquitousTypes exposing (LocalCoords)
import Utils exposing (gradientColourPastel, gradientColourVivid)
import Vector3d
import ViewTypes exposing (ViewingMode(..))


optionally : Bool -> List (Entity LocalCoords) -> List (Entity LocalCoords)
optionally test element =
    if test then
        element

    else
        []


makeStatic3DEntities :
    RenderingContext
    -> List DrawingRoad
    -> List (Entity LocalCoords)
makeStatic3DEntities context roadList =
    let
        seaLevel =
            let
                bigger =
                    BoundingBox3d.expandBy (Length.meters 1000.0) context.nodeBox

                { minX, maxX, minY, maxY, minZ, maxZ } =
                    BoundingBox3d.extrema bigger

                showPlane =
                    if context.displayOptions.seaLevel then
                        Length.meters 0.0

                    else
                        Length.meters <| Length.inMeters minZ + 990.0
            in
            Scene3d.quad (Material.color Color.darkGreen)
                (Point3d.xyz minX minY showPlane)
                (Point3d.xyz minX maxY showPlane)
                (Point3d.xyz maxX maxY showPlane)
                (Point3d.xyz maxX minY showPlane)

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        brownPillar loc =
            cylinder (Material.color Color.brown) <|
                Cylinder3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 -1.0)
                        loc
                    )
                    negativeZ
                    { radius = meters 0.5
                    , length = Quantity.minus (Point3d.zCoordinate loc) (Length.meters 1.0)
                    }

        pillars =
            List.map
                (\r -> brownPillar r.startsAt.xyz)
                (List.take 1 roadList)
                ++ List.map
                    (\r -> brownPillar r.endsAt.xyz)
                    roadList

        trackpointmarker loc =
            cone (Material.color Color.black) <|
                Cone3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 -1.0)
                        loc
                    )
                    positiveZ
                    { radius = meters <| 0.6
                    , length = meters <| 1.0
                    }

        trackpointMarkers =
            let
                makeStartCone road =
                    trackpointmarker road.startsAt.xyz

                makeEndCone road =
                    trackpointmarker road.endsAt.xyz
            in
            List.map makeStartCone (List.take 1 roadList)
                ++ List.map makeEndCone roadList

        roadSurfaces =
            List.concatMap roadSurface <|
                roadList

        roadSurface road =
            let
                roadAsSegment =
                    LineSegment3d.fromEndpoints ( road.startsAt.xyz, road.endsAt.xyz )

                roadSegmentInXY =
                    LineSegment3d.projectOnto Plane3d.xy roadAsSegment

                roadVector =
                    Vector3d.from
                        (LineSegment3d.startPoint roadSegmentInXY)
                        (LineSegment3d.endPoint roadSegmentInXY)

                leftKerbVector =
                    Vector3d.scaleTo (Length.meters 3.0) <|
                        Vector3d.rotateAround Axis3d.z (Angle.degrees -90) roadVector

                rightKerbVector =
                    Vector3d.reverse leftKerbVector

                ( leftKerb, rightKerb ) =
                    ( LineSegment3d.translateBy leftKerbVector roadAsSegment
                    , LineSegment3d.translateBy rightKerbVector roadAsSegment
                    )
            in
            [ Scene3d.quad (Material.matte Color.grey)
                (LineSegment3d.startPoint leftKerb)
                (LineSegment3d.endPoint leftKerb)
                (LineSegment3d.endPoint rightKerb)
                (LineSegment3d.startPoint rightKerb)
            ]

        subtleGradientLine road =
            let
                ( halfX, halfY ) =
                    -- Width of the centre line.
                    ( 0.3 * cos road.bearing
                    , 0.3 * sin road.bearing
                    )

                roadAsSegment =
                    LineSegment3d.fromEndpoints ( road.startsAt.xyz, road.endsAt.xyz )

                leftVector =
                    Vector3d.meters
                        (-1.0 * halfX)
                        halfY
                        0.05

                rightVector =
                    Vector3d.reverse leftVector

                ( leftEdge, rightEdge ) =
                    ( LineSegment3d.translateBy leftVector roadAsSegment
                    , LineSegment3d.translateBy rightVector roadAsSegment
                    )
            in
            [ --surface
              Scene3d.quad (Material.color <| gradientColourPastel road.gradient)
                (LineSegment3d.startPoint leftEdge)
                (LineSegment3d.endPoint leftEdge)
                (LineSegment3d.endPoint rightEdge)
                (LineSegment3d.startPoint rightEdge)
            ]

        centreLine =
            List.concatMap subtleGradientLine <|
                roadList

        curtains =
            List.concatMap curtain <|
                roadList

        curtainColour gradient =
            case context.displayOptions.curtainStyle of
                RainbowCurtain ->
                    gradientColourVivid gradient

                PastelCurtain ->
                    gradientColourPastel gradient

                PlainCurtain ->
                    Color.rgb255 0 100 0

                NoCurtain ->
                    Color.rgb255 0 0 0

        curtain road =
            [ Scene3d.quad (Material.color <| curtainColour road.gradient)
                road.startsAt.xyz
                road.endsAt.xyz
                (Point3d.projectOnto Plane3d.xy road.endsAt.xyz)
                (Point3d.projectOnto Plane3d.xy road.startsAt.xyz)
            ]

        graphNodeCircles =
            List.map
                (\node ->
                    cone (Material.color Color.blue) <|
                        Cone3d.startingAt
                            node.xyz
                            positiveZ
                            { radius = meters <| 5.0
                            , length = meters <| 5.0
                            }
                )
                context.graphNodes
    in
    [ seaLevel ]
        ++ optionally context.displayOptions.roadPillars pillars
        ++ optionally context.displayOptions.roadCones trackpointMarkers
        ++ optionally context.displayOptions.roadTrack roadSurfaces
        ++ optionally (context.displayOptions.curtainStyle /= NoCurtain) curtains
        ++ optionally context.displayOptions.centreLine centreLine



--++ graphNodeCircles


exaggerateRoad context road =
    { road
        | profileStartsAt = exaggerateNode context road.profileStartsAt
        , profileEndsAt = exaggerateNode context road.profileEndsAt
    }


exaggerateNode context node =
    { node
        | xyz =
            Point3d.scaleAbout
                (Point3d.projectOntoAxis Axis3d.y node.xyz)
                context.verticalExaggeration
                node.xyz
    }


makeStaticProfileEntities : RenderingContext -> List DrawingRoad -> List (Entity LocalCoords)
makeStaticProfileEntities context beforeExaggeration =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    -- We manipulate the context to get the scaling right.
    -- Decided to duplicate the function so we can have different shapes to suit the view.
    let
        roadList =
            List.map (exaggerateRoad context) beforeExaggeration

        brownPillar loc =
            cylinder (Material.color Color.brown) <|
                Cylinder3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 -0.2)
                        loc
                    )
                    negativeZ
                    { radius = meters 0.1
                    , length = Quantity.minus (Point3d.zCoordinate loc) (Length.meters 0.2)
                    }

        pillars =
            List.map
                (\r -> brownPillar r.profileStartsAt.xyz)
                (List.take 1 roadList)
                ++ List.map
                    (\r -> brownPillar r.profileEndsAt.xyz)
                    roadList

        trackpointmarker loc =
            sphere (Material.color Color.black) <|
                Sphere3d.withRadius
                    (meters 0.2)
                    loc

        trackpointMarkers =
            List.map
                (\r -> trackpointmarker r.profileStartsAt.xyz)
                (List.take 1 roadList)
                ++ List.map
                    (\r -> trackpointmarker r.profileEndsAt.xyz)
                    roadList

        curtains =
            List.concat <|
                List.map curtain <|
                    roadList

        curtainColour gradient =
            case context.displayOptions.curtainStyle of
                RainbowCurtain ->
                    gradientColourVivid gradient

                PastelCurtain ->
                    gradientColourPastel gradient

                PlainCurtain ->
                    Color.rgb255 0 100 0

                NoCurtain ->
                    Color.rgb255 0 0 0

        curtain road =
            [ Scene3d.quad (Material.color <| curtainColour road.gradient)
                road.profileStartsAt.xyz
                road.profileEndsAt.xyz
                (Point3d.projectOnto Plane3d.xy road.profileEndsAt.xyz)
                (Point3d.projectOnto Plane3d.xy road.profileStartsAt.xyz)
            ]
    in
    []
        ++ optionally context.displayOptions.roadPillars pillars
        ++ optionally context.displayOptions.roadCones trackpointMarkers
        ++ optionally (context.displayOptions.curtainStyle /= NoCurtain) curtains


makeVaryingVisualEntities : RenderingContext -> Array DrawingRoad -> List (Entity LocalCoords)
makeVaryingVisualEntities context _ =
    let
        currentPositionDisc =
            case ( context.currentNode, context.viewingMode ) of
                ( Just node, ThirdPersonView ) ->
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                node.xyz
                            )
                            negativeZ
                            { radius = meters <| 3.0
                            , length = meters <| 10.0
                            }
                    ]

                ( Just node, PlanView ) ->
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                node.xyz
                            )
                            negativeZ
                            { radius = meters <| 1.5
                            , length = meters <| 10.0
                            }
                    ]

                _ ->
                    []

        markedNode =
            case ( context.markedNode, context.viewingMode ) of
                ( Just node, ThirdPersonView ) ->
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                node.xyz
                            )
                            negativeZ
                            { radius = meters <| 3.5
                            , length = meters <| 8.0
                            }
                    ]

                ( Just node, PlanView ) ->
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                node.xyz
                            )
                            negativeZ
                            { radius = meters <| 1.6
                            , length = meters <| 8.0
                            }
                    ]

                _ ->
                    []

        bendAsRoadSegments bend =
            List.map2
                (\n1 n2 ->
                    { startsAt = { xyz = n1 }
                    , endsAt = { xyz = n2 }
                    , bearing =
                        case Direction3d.from n1 n2 of
                            Just direction ->
                                atan2 (xComponent direction) (yComponent direction)

                            Nothing ->
                                0.0
                    }
                )
                context.smoothedBend
                (List.drop 1 context.smoothedBend)

        suggestedBend =
            List.map (nudgeElement Color.lightYellow) (bendAsRoadSegments context.smoothedBend)

        nudges =
            List.map (nudgeElement Color.lightOrange) context.nudgedRoads

        nudgeElement colour road =
            let
                ( halfX, halfY ) =
                    -- Width of the line.
                    ( 1.0 * cos road.bearing
                    , 1.0 * sin road.bearing
                    )

                roadAsSegment =
                    LineSegment3d.fromEndpoints ( road.startsAt.xyz, road.endsAt.xyz )
                        |> LineSegment3d.translateBy (Vector3d.meters 0.0 0.0 0.1)

                leftVector =
                    Vector3d.meters
                        (-1.0 * halfX)
                        halfY
                        0.05

                rightVector =
                    Vector3d.reverse leftVector

                ( leftEdge, rightEdge ) =
                    ( LineSegment3d.translateBy leftVector roadAsSegment
                    , LineSegment3d.translateBy rightVector roadAsSegment
                    )
            in
            Scene3d.quad (Material.color colour)
                (LineSegment3d.startPoint leftEdge)
                (LineSegment3d.endPoint leftEdge)
                (LineSegment3d.endPoint rightEdge)
                (LineSegment3d.startPoint rightEdge)
    in
    currentPositionDisc
        ++ markedNode
        ++ suggestedBend
        ++ nudges


makeVaryingProfileEntities : RenderingContext -> List DrawingRoad -> List (Entity LocalCoords)
makeVaryingProfileEntities context beforeExaggeration =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    -- We might draw things differently to suit the projection.
    let
        roadList =
            List.map (exaggerateRoad context) beforeExaggeration

        triangleForNode : TrackPoint -> Triangle3d Length.Meters LocalCoords
        triangleForNode node =
            let
                relativeSize =
                    2.0 ^ (14.0 - context.zoomLevel)

                apex =
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 0.3)
                        node.xyz

                leftTop =
                    Point3d.translateBy
                        (Vector3d.meters 0.0 (-0.3 * relativeSize) relativeSize)
                        apex

                rightTop =
                    Point3d.translateBy
                        (Vector3d.meters 0.0 (0.3 * relativeSize) relativeSize)
                        apex
            in
            Triangle3d.fromVertices
                ( leftTop, rightTop, apex )

        currentPositionDisc =
            -- Try a triangle instead of a cone.
            case context.currentNode of
                Just node ->
                    [ triangle
                        (Material.color Color.lightOrange)
                        (triangleForNode <| exaggerateNode context node)
                    ]

                _ ->
                    []

        markedNode =
            case context.markedNode of
                Just node ->
                    [ triangle
                        (Material.color Color.purple)
                        (triangleForNode <| exaggerateNode context node)
                    ]

                _ ->
                    []

        nudgedNodes =
            -- Slightly tricky as we have to correlate nudged roads with the unrolled profile.
            case context.nudgedRegionStart of
                Just node1 ->
                    let
                        baselineWithElevationFromNudged baseline nudged =
                            let
                                baselineRecord =
                                    Point3d.toMeters baseline

                                nudgedRecord =
                                    Point3d.toMeters nudged
                            in
                            Point3d.fromMeters
                                { baselineRecord | z = nudgedRecord.z * context.verticalExaggeration }

                        blendTheRoadData baseline nudged =
                            Scene3d.quad (Material.color Color.lightYellow)
                                baseline.profileStartsAt.xyz
                                (baselineWithElevationFromNudged
                                    baseline.profileStartsAt.xyz
                                    nudged.startsAt.xyz
                                )
                                (baselineWithElevationFromNudged
                                    baseline.profileEndsAt.xyz
                                    nudged.endsAt.xyz
                                )
                                baseline.profileEndsAt.xyz

                        segmentsInvolved =
                            -- If the lowest marker is not at zero, then
                            -- the nudged roads includes the "on-ramp" from
                            -- the previous node, as well as the "off-ramp"
                            -- after the second node.
                            List.drop (node1 - 1) roadList

                        nudgedRoads =
                            -- Combine the nudged roads cleverly with our unrolled ones
                            List.map2
                                blendTheRoadData
                                segmentsInvolved
                            <|
                                List.map (exaggerateRoad context) context.nudgedRoads
                    in
                    nudgedRoads

                Nothing ->
                    []
    in
    currentPositionDisc
        ++ markedNode
        ++ nudgedNodes
