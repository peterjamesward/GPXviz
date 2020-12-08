module VisualEntities exposing (..)

import Array exposing (Array)
import BoundingBox3d
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (CurtainStyle(..), DisplayOptions)
import Length exposing (meters)
import LineSegment3d
import NodesAndRoads exposing (DrawingNode, DrawingRoad, LocalCoords)
import Plane3d
import Point3d
import Quantity
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity, cone, cylinder, sphere)
import Scene3d.Material as Material
import Sphere3d
import Utils exposing (gradientColourPastel, gradientColourVivid)
import Vector3d
import ViewTypes exposing (ViewSubmode(..), ViewingMode(..))


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
            in
            Scene3d.quad (Material.color Color.darkGreen)
                (Point3d.xyz minX minY (Length.meters 0.0))
                (Point3d.xyz minX maxY (Length.meters 0.0))
                (Point3d.xyz maxX maxY (Length.meters 0.0))
                (Point3d.xyz maxX minY (Length.meters 0.0))

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
                (\r -> brownPillar r.startsAt.location)
                (List.take 1 roadList)
                ++ List.map
                    (\r -> brownPillar r.endsAt.location)
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
                    trackpointmarker road.startsAt.location

                makeEndCone road =
                    trackpointmarker road.endsAt.location
            in
            List.map makeStartCone (List.take 1 roadList)
                ++ List.map makeEndCone roadList

        roadSurfaces =
            List.concat <|
                List.map roadSurface <|
                    roadList

        roadSurface road =
            let
                edgeHeight =
                    -- Let's try a low wall at the road's edges.
                    0.3

                ( kerbX, kerbY ) =
                    -- Road is assumed to be 6 m wide.
                    ( 3.0 * cos road.bearing
                    , 3.0 * sin road.bearing
                    )

                roadAsSegment =
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

                ---- kerb walls
                --, Scene3d.quad (Material.color Color.darkGrey)
                --    (Point3d.meters (x1 + kerbX) (y1 - kerbY) z1)
                --    (Point3d.meters (x2 + kerbX) (y2 - kerbY) z2)
                --    (Point3d.meters (x2 + kerbX) (y2 - kerbY) (z2 + edgeHeight))
                --    (Point3d.meters (x1 + kerbX) (y1 - kerbY) (z1 + edgeHeight))
                --, Scene3d.quad (Material.color Color.darkGrey)
                --    (Point3d.meters (x1 - kerbX) (y1 + kerbY) z1)
                --    (Point3d.meters (x2 - kerbX) (y2 + kerbY) z2)
                --    (Point3d.meters (x2 - kerbX) (y2 + kerbY) (z2 + edgeHeight))
                --    (Point3d.meters (x1 - kerbX) (y1 + kerbY) (z1 + edgeHeight))
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
                    LineSegment3d.fromEndpoints ( road.startsAt.location, road.endsAt.location )

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
            List.concat <|
                List.map subtleGradientLine <|
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
                road.startsAt.location
                road.endsAt.location
                (Point3d.projectOnto Plane3d.xy road.endsAt.location)
                (Point3d.projectOnto Plane3d.xy road.startsAt.location)
            ]

        optionally : Bool -> List (Entity LocalCoords) -> List (Entity LocalCoords)
        optionally test element =
            if test then
                element

            else
                []
    in
    seaLevel
        :: optionally context.displayOptions.roadPillars pillars
        ++ optionally context.displayOptions.roadCones trackpointMarkers
        ++ optionally context.displayOptions.roadTrack roadSurfaces
        ++ optionally (context.displayOptions.curtainStyle /= NoCurtain) curtains
        ++ optionally context.displayOptions.centreLine centreLine


makeStaticProfileEntities : RenderingContext -> List DrawingRoad -> List (Entity LocalCoords)
makeStaticProfileEntities context roadList =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    -- We manipulate the context to get the scaling right.
    -- Decided to duplicate the function so we can have different shapes to suit the view.
    let
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
                (\r -> brownPillar r.startsAt.location)
                (List.take 1 roadList)
                ++ List.map
                    (\r -> brownPillar r.endsAt.location)
                    roadList

        trackpointmarker loc =
            sphere (Material.color Color.black) <|
                Sphere3d.withRadius
                    (meters 0.2)
                    loc

        trackpointMarkers =
            List.map
                (\r -> trackpointmarker r.startsAt.location)
                (List.take 1 roadList)
                ++ List.map
                    (\r -> trackpointmarker r.endsAt.location)
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
                road.startsAt.location
                road.endsAt.location
                (Point3d.projectOnto Plane3d.xy road.endsAt.location)
                (Point3d.projectOnto Plane3d.xy road.startsAt.location)
            ]

        optionally : Bool -> List (Entity LocalCoords) -> List (Entity LocalCoords)
        optionally test element =
            if test then
                element

            else
                []
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
                ( Just road, ThirdPersonView ) ->
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                road.startsAt.location
                            )
                            negativeZ
                            { radius = meters <| 3.0
                            , length = meters <| 10.0
                            }
                    ]

                ( Just road, PlanView ) ->
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                road.startsAt.location
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
                ( Just road, ThirdPersonView ) ->
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                road.startsAt.location
                            )
                            negativeZ
                            { radius = meters <| 3.5
                            , length = meters <| 8.0
                            }
                    ]

                ( Just road, PlanView ) ->
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 10.1)
                                road.startsAt.location
                            )
                            negativeZ
                            { radius = meters <| 1.6
                            , length = meters <| 8.0
                            }
                    ]

                _ ->
                    []

        suggestedBend =
            if context.viewingSubMode == ShowBendFixes then
                List.map bendElement context.smoothedBend

            else
                []

        bendElement road =
            let
                ( halfX, halfY ) =
                    -- Width of the line.
                    ( 1.0 * cos road.bearing
                    , 1.0 * sin road.bearing
                    )

                roadAsSegment =
                    LineSegment3d.fromEndpoints ( road.startsAt.location, road.endsAt.location )
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
            Scene3d.quad (Material.color Color.lightYellow)
                (LineSegment3d.startPoint leftEdge)
                (LineSegment3d.endPoint leftEdge)
                (LineSegment3d.endPoint rightEdge)
                (LineSegment3d.startPoint rightEdge)
    in
    currentPositionDisc
        ++ markedNode
        ++ suggestedBend


makeVaryingProfileEntities : RenderingContext -> List DrawingRoad -> List (Entity LocalCoords)
makeVaryingProfileEntities context _ =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    let
        currentPositionDisc =
            case ( context.currentNode, context.viewingMode ) of
                ( Just road, ProfileView ) ->
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 2.0)
                                road.startsAt.location
                            )
                            negativeZ
                            { radius = meters 0.3
                            , length = meters 1.9
                            }
                    ]

                _ ->
                    []

        markedNode =
            case ( context.markedNode, context.viewingMode ) of
                ( Just road, ProfileView ) ->
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 2.1)
                                road.startsAt.location
                            )
                            negativeZ
                            { radius = meters 0.25
                            , length = meters 2.0
                            }
                    ]

                _ ->
                    []
    in
    currentPositionDisc
        ++ markedNode
