module VisualEntities exposing (..)

import Array exposing (Array)
import BoundingBox2d
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (CurtainStyle(..), DisplayOptions)
import Length exposing (meters)
import NodesAndRoads exposing (DrawingNode, DrawingRoad, MyCoord)
import Point2d
import Point3d
import RenderingContext exposing (RenderingContext)
import ScalingInfo exposing (ScalingInfo)
import Scene3d exposing (Entity, cone, cylinder, sphere)
import Scene3d.Material as Material
import Sphere3d
import Spherical exposing (metresPerDegreeLatitude)
import TrackPoint exposing (TrackPoint)
import Utils exposing (gradientColourPastel, gradientColourVivid)
import ViewTypes exposing (ViewSubmode(..), ViewingMode(..))


roadMapper road =
    ( ( road.startsAt.x, road.startsAt.y, road.startsAt.z )
    , ( road.endsAt.x, road.endsAt.y, road.endsAt.z )
    )


makeStatic3DEntities :
    RenderingContext
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeStatic3DEntities context roads =
    let
        roadList =
            Array.toList roads

        pointList =
            List.map
                (\r -> Point2d.meters r.startsAt.x r.startsAt.y)
                roadList

        maybeBoundingBox =
            -- TODO: This should live in ScalingInfo.
            BoundingBox2d.hullN pointList

        seaLevel =
            case maybeBoundingBox of
                Just box ->
                    let
                        bigger =
                            BoundingBox2d.expandBy (Length.meters 1000.0) box

                        { minX, maxX, minY, maxY } =
                            BoundingBox2d.extrema bigger
                    in
                    Scene3d.quad (Material.color Color.darkGreen)
                        (Point3d.xyz minX minY (Length.meters 0.0))
                        (Point3d.xyz minX maxY (Length.meters 0.0))
                        (Point3d.xyz maxX maxY (Length.meters 0.0))
                        (Point3d.xyz maxX minY (Length.meters 0.0))

                Nothing ->
                    Scene3d.quad (Material.color Color.darkGreen)
                        (Point3d.meters 0.0 0.0 0.0)
                        (Point3d.meters 0.0 0.0 0.0)
                        (Point3d.meters 0.0 0.0 0.0)
                        (Point3d.meters 0.0 0.0 0.0)

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        brownPillar x y z =
            cylinder (Material.color Color.brown) <|
                Cylinder3d.startingAt
                    (Point3d.meters x y (z - 1.0))
                    negativeZ
                    { radius = meters <| 0.5
                    , length = meters <| z - 1.0
                    }

        pillars =
            let
                makeStartPillar road =
                    let
                        ( _, ( x2, y2, z2 ) ) =
                            roadMapper road
                    in
                    brownPillar x2 y2 z2

                makeEndPillar road =
                    let
                        ( _, ( x2, y2, z2 ) ) =
                            roadMapper road
                    in
                    brownPillar x2 y2 z2
            in
            List.map makeStartPillar (List.take 1 roadList)
                ++ List.map makeEndPillar roadList

        trackpointmarker x y z =
            cone (Material.color Color.black) <|
                Cone3d.startingAt
                    (Point3d.meters x y (z - 1.0))
                    positiveZ
                    { radius = meters <| 0.6
                    , length = meters <| 1.0
                    }

        trackpointMarkers =
            let
                makeStartCone road =
                    let
                        ( ( x1, y1, z1 ), _ ) =
                            roadMapper road
                    in
                    trackpointmarker x1 y1 z1

                makeEndCone road =
                    let
                        ( _, ( x2, y2, z2 ) ) =
                            roadMapper road
                    in
                    trackpointmarker x2 y2 z2
            in
            List.map makeStartCone (List.take 1 roadList)
                ++ List.map makeEndCone roadList

        roadSurfaces =
            List.concat <|
                List.map roadSurface <|
                    roadList

        roadSurface segment =
            let
                kerbX =
                    -- Road is assumed to be 6 m wide.
                    3.0 * cos segment.bearing

                kerbY =
                    3.0 * sin segment.bearing

                edgeHeight =
                    -- Let's try a low wall at the road's edges.
                    0.3

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            [ --surface
              Scene3d.quad (Material.matte Color.grey)
                (Point3d.meters (x1 + kerbX) (y1 - kerbY) z1)
                (Point3d.meters (x2 + kerbX) (y2 - kerbY) z2)
                (Point3d.meters (x2 - kerbX) (y2 + kerbY) z2)
                (Point3d.meters (x1 - kerbX) (y1 + kerbY) z1)

            -- kerb walls
            , Scene3d.quad (Material.color Color.darkGrey)
                (Point3d.meters (x1 + kerbX) (y1 - kerbY) z1)
                (Point3d.meters (x2 + kerbX) (y2 - kerbY) z2)
                (Point3d.meters (x2 + kerbX) (y2 - kerbY) (z2 + edgeHeight))
                (Point3d.meters (x1 + kerbX) (y1 - kerbY) (z1 + edgeHeight))
            , Scene3d.quad (Material.color Color.darkGrey)
                (Point3d.meters (x1 - kerbX) (y1 + kerbY) z1)
                (Point3d.meters (x2 - kerbX) (y2 + kerbY) z2)
                (Point3d.meters (x2 - kerbX) (y2 + kerbY) (z2 + edgeHeight))
                (Point3d.meters (x1 - kerbX) (y1 + kerbY) (z1 + edgeHeight))
            ]

        subtleGradientLine segment =
            let
                halfX =
                    -- Road is assumed to be 6 m wide.
                    0.3 * cos segment.bearing

                halfY =
                    0.3 * sin segment.bearing

                raised =
                    0.05

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            [ --surface
              Scene3d.quad (Material.color <| gradientColourPastel segment.gradient)
                (Point3d.meters (x1 + halfX) (y1 - halfY) (z1 + raised))
                (Point3d.meters (x2 + halfX) (y2 - halfY) (z2 + raised))
                (Point3d.meters (x2 - halfX) (y2 + halfY) (z2 + raised))
                (Point3d.meters (x1 - halfX) (y1 + halfY) (z1 + raised))
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

        curtain segment =
            let
                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            [ Scene3d.quad (Material.color <| curtainColour segment.gradient)
                (Point3d.meters x1 y1 z1)
                (Point3d.meters x2 y2 z2)
                (Point3d.meters x2 y2 0.0)
                (Point3d.meters x1 y1 0.0)
            ]

        optionally : Bool -> List (Entity MyCoord) -> List (Entity MyCoord)
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


makeStaticProfileEntities : RenderingContext -> List DrawingRoad -> List (Entity MyCoord)
makeStaticProfileEntities context roadList =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    -- We manipulate the context to get the scaling right.
    -- Decided to duplicate the function so we can have different shapes to suit the view.
    let
        seaLevelInClipSpace =
            0.0

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        brownPillar x y z =
            cylinder (Material.color Color.brown) <|
                Cylinder3d.startingAt
                    (Point3d.meters x y (z - 0.2))
                    negativeZ
                    { radius = meters <| 0.1
                    , length = meters <| z - 0.2 - seaLevelInClipSpace
                    }

        pillars =
            let
                makeStartPillar road =
                    let
                        ( ( x1, y1, z1 ), _ ) =
                            roadMapper road
                    in
                    brownPillar x1 y1 z1

                makeEndPillar road =
                    let
                        ( _, ( x2, y2, z2 ) ) =
                            roadMapper road
                    in
                    brownPillar x2 y2 z2
            in
            List.map makeStartPillar (List.take 1 roadList)
                ++ List.map makeEndPillar roadList

        trackpointmarker x y z =
            sphere (Material.color Color.black) <|
                Sphere3d.withRadius
                    (meters <| 0.1)
                    (Point3d.meters x y z)

        trackpointMarkers =
            let
                makeStartCone road =
                    let
                        ( ( x1, y1, z1 ), _ ) =
                            roadMapper road
                    in
                    trackpointmarker x1 y1 z1

                makeEndCone road =
                    let
                        ( _, ( x2, y2, z2 ) ) =
                            roadMapper road
                    in
                    trackpointmarker x2 y2 z2
            in
            List.map makeStartCone (List.take 1 roadList)
                ++ List.map makeEndCone roadList

        roadSurfaces =
            List.concat <|
                List.map roadSurface <|
                    roadList

        roadSurface _ =
            []

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

        curtain segment =
            let
                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            [ Scene3d.quad (Material.color <| curtainColour segment.gradient)
                (Point3d.meters x1 y1 z1)
                (Point3d.meters x2 y2 z2)
                (Point3d.meters x2 y2 seaLevelInClipSpace)
                (Point3d.meters x1 y1 seaLevelInClipSpace)
            ]

        optionally : Bool -> List (Entity MyCoord) -> List (Entity MyCoord)
        optionally test element =
            if test then
                element

            else
                []
    in
    optionally context.displayOptions.roadPillars pillars
        ++ optionally context.displayOptions.roadCones trackpointMarkers
        ++ optionally context.displayOptions.roadTrack roadSurfaces
        ++ optionally (context.displayOptions.curtainStyle /= NoCurtain) curtains


makeVaryingVisualEntities : RenderingContext -> Array DrawingRoad -> List (Entity MyCoord)
makeVaryingVisualEntities context _ =
    let
        currentPositionDisc =
            case ( context.currentNode, context.viewingMode ) of
                ( Just road, ThirdPersonView ) ->
                    let
                        ( ( x, y, z ), _ ) =
                            roadMapper road
                    in
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.meters x y (z + 10.1))
                            negativeZ
                            { radius = meters <| 3.0
                            , length = meters <| 10.0
                            }
                    ]

                ( Just road, PlanView ) ->
                    let
                        ( ( x, y, z ), _ ) =
                            roadMapper road
                    in
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.meters x y (z + 10.1))
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
                    let
                        ( ( x, y, z ), _ ) =
                            roadMapper road
                    in
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.meters x y (z + 10.1))
                            negativeZ
                            { radius = meters <| 3.5
                            , length = meters <| 8.0
                            }
                    ]

                ( Just road, PlanView ) ->
                    let
                        ( ( x, y, z ), _ ) =
                            roadMapper road
                    in
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.meters x y (z + 10.1))
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

        bendElement segment =
            let
                kerbX =
                    1.0 * cos segment.bearing

                kerbY =
                    1.0 * sin segment.bearing

                floatHeight =
                    0.1

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            Scene3d.quad (Material.color Color.lightYellow)
                (Point3d.meters (x1 + kerbX) (y1 - kerbY) (z1 + floatHeight))
                (Point3d.meters (x2 + kerbX) (y2 - kerbY) (z2 + floatHeight))
                (Point3d.meters (x2 - kerbX) (y2 + kerbY) (z2 + floatHeight))
                (Point3d.meters (x1 - kerbX) (y1 + kerbY) (z1 + floatHeight))
    in
    currentPositionDisc
        ++ markedNode
        ++ suggestedBend


makeVaryingProfileEntities : RenderingContext -> List DrawingRoad -> List (Entity MyCoord)
makeVaryingProfileEntities context _ =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    let
        currentPositionDisc =
            case ( context.currentNode, context.viewingMode ) of
                ( Just road, ProfileView ) ->
                    let
                        ( ( x, y, z ), _ ) =
                            roadMapper road
                    in
                    [ cone (Material.color Color.lightOrange) <|
                        Cone3d.startingAt
                            (Point3d.meters x y (z + 2.0))
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
                    let
                        ( ( x, y, z ), _ ) =
                            roadMapper road
                    in
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.meters x y (z + 2.0))
                            negativeZ
                            { radius = meters 0.3
                            , length = meters 1.9
                            }
                    ]

                _ ->
                    []

        suggestedBend =
            if context.viewingSubMode == ShowBendFixes then
                List.map bendElement context.smoothedBend

            else
                []

        bendElement segment =
            let
                kerbX =
                    2.0 * cos segment.bearing

                kerbY =
                    2.0 * sin segment.bearing

                floatHeight =
                    0.1

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            Scene3d.quad (Material.color Color.white)
                (Point3d.meters (x1 + kerbX) (y1 - kerbY) (z1 + floatHeight))
                (Point3d.meters (x2 + kerbX) (y2 - kerbY) (z2 + floatHeight))
                (Point3d.meters (x2 - kerbX) (y2 + kerbY) (z2 + floatHeight))
                (Point3d.meters (x1 - kerbX) (y1 + kerbY) (z1 + floatHeight))
    in
    currentPositionDisc
        ++ markedNode
        ++ suggestedBend


deriveScalingInfo : List TrackPoint -> ScalingInfo
deriveScalingInfo tps =
    let
        lowerBounds tp =
            { lat = List.foldl min 90.0 <| List.map .lat tp
            , lon = List.foldl min 180.0 <| List.map .lon tp
            , ele = List.foldl min 10000.0 <| List.map .ele tp
            , idx = 0
            }

        upperBounds tp =
            { lat = List.foldl max -90.0 <| List.map .lat tp
            , lon = List.foldl max -180.0 <| List.map .lon tp
            , ele = List.foldl max -10000.0 <| List.map .ele tp
            , idx = 0
            }

        mins =
            lowerBounds tps

        maxs =
            upperBounds tps

        findCentres =
            { lat = (mins.lat + maxs.lat) / 2.0
            , lon = (mins.lon + maxs.lon) / 2.0
            , ele = (mins.ele + maxs.ele) / 2.0
            , idx = 0
            }

        scalingFactor =
            max (abs (maxs.lat - mins.lat))
                (abs (maxs.lon - mins.lon))

        metresToClipSpace =
            1 / (0.5 * scalingFactor * metresPerDegreeLatitude)

        elevationToClipSpace e =
            (e - findCentres.ele) * metresToClipSpace
    in
    { mins = mins
    , maxs = maxs
    , centres = findCentres
    }
