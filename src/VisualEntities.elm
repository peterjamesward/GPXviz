module VisualEntities exposing
    ( ThingsWeNeedForRendering
    , deriveScalingInfo
    , makeStatic3DEntities
    , makeStaticProfileEntities
    , makeVaryingVisualEntities
    )

import Array exposing (Array)
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (CurtainStyle(..), DisplayOptions)
import Length exposing (meters)
import List exposing (take)
import NodesAndRoads exposing (DrawingNode, DrawingRoad, MyCoord)
import Point3d
import ScalingInfo exposing (ScalingInfo)
import Scene3d exposing (Entity, cone, cylinder)
import Scene3d.Material as Material
import Spherical exposing (metresPerDegreeLatitude)
import TrackPoint exposing (TrackPoint)
import Utils exposing (gradientColourPastel, gradientColourVivid)
import ViewTypes exposing (ThirdPersonSubmode(..), ViewingMode(..))


type alias ThingsWeNeedForRendering =
    { displayOptions : DisplayOptions
    , currentNode : Maybe DrawingRoad
    , markedNode : Maybe DrawingRoad
    , scaling : ScalingInfo
    , viewingMode : ViewingMode
    , viewingSubMode : ThirdPersonSubmode
    , smoothedBend : List DrawingRoad
    }


segmentDirection segment =
    let
        maybe =
            Direction3d.from
                (Point3d.meters
                    segment.startsAt.x
                    segment.startsAt.y
                    segment.startsAt.z
                )
                (Point3d.meters
                    segment.endsAt.x
                    segment.endsAt.y
                    segment.endsAt.z
                )
    in
    case maybe of
        Just dir ->
            dir

        Nothing ->
            positiveZ


type alias NodeMapping =
    DrawingNode -> ( Float, Float, Float )


type alias RoadMapping =
    DrawingRoad -> ( ( Float, Float, Float ), ( Float, Float, Float ) )


preserve3Dspace : RoadMapping
preserve3Dspace road =
    ( ( road.startsAt.x, road.startsAt.y, road.startsAt.z )
    , ( road.endsAt.x, road.endsAt.y, road.endsAt.z )
    )


makeStatic3DEntities :
    ThingsWeNeedForRendering
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeStatic3DEntities context roads =
    makeStaticVisualEntities context
        preserve3Dspace
        roads


makeStaticVisualEntities :
    ThingsWeNeedForRendering
    -> RoadMapping
    -> Array DrawingRoad
    -> List (Entity MyCoord)
makeStaticVisualEntities context roadMapper roads =
    let
        roadList =
            Array.toList roads

        seaLevelInClipSpace =
            context.scaling.seaLevelInClipSpace

        seaLevel =
            Scene3d.quad (Material.color Color.darkGreen)
                (Point3d.meters -1.2 -1.2 seaLevelInClipSpace)
                (Point3d.meters 1.2 -1.2 seaLevelInClipSpace)
                (Point3d.meters 1.2 1.2 seaLevelInClipSpace)
                (Point3d.meters -1.2 1.2 seaLevelInClipSpace)

        metresToClipSpace =
            context.scaling.metresToClipSpace

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        brownPillar x y z =
            cylinder (Material.color Color.brown) <|
                Cylinder3d.startingAt
                    (Point3d.meters x y (z - 1.0 * metresToClipSpace))
                    negativeZ
                    { radius = meters <| 0.5 * metresToClipSpace
                    , length = meters <| z - 1.0 * metresToClipSpace - seaLevelInClipSpace
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
                    (Point3d.meters x y (z - 1.0 * metresToClipSpace))
                    positiveZ
                    { radius = meters <| 0.6 * metresToClipSpace
                    , length = meters <| 1.0 * metresToClipSpace
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
                    3.0 * cos segment.bearing * metresToClipSpace

                kerbY =
                    3.0 * sin segment.bearing * metresToClipSpace

                edgeHeight =
                    -- Let's try a low wall at the road's edges.
                    0.3 * metresToClipSpace

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    roadMapper segment
            in
            [ --surface
              Scene3d.quad (Material.color Color.grey)
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
    seaLevel
        :: optionally context.displayOptions.roadPillars pillars
        ++ optionally context.displayOptions.roadCones trackpointMarkers
        ++ optionally context.displayOptions.roadTrack roadSurfaces
        ++ optionally (context.displayOptions.curtainStyle /= NoCurtain) curtains


makeStaticProfileEntities : ThingsWeNeedForRendering -> Array DrawingRoad -> List (Entity MyCoord)
makeStaticProfileEntities context roads =
    -- Same thing as above but "unrolled" view of road for viewing profile.
    -- We manipulate the context to get the scaling right.
    let
        roadList =
            Array.toList roads

        totalLength =
            Maybe.withDefault 1.0 <|
                List.maximum <|
                    List.map .endDistance roadList

        unrolledRoads : List DrawingRoad
        unrolledRoads =
            List.map unrollRoad roadList

        unrollRoad : DrawingRoad -> DrawingRoad
        unrollRoad road =
            let
                startNode =
                    road.startsAt

                endNode =
                    road.endsAt

                newStartNode =
                    { startNode
                        | y = 2.0 * road.startDistance / totalLength - 1.0
                        , x = 0.0
                    }

                newEndNode =
                    { endNode
                        | y = 2.0 * road.endDistance / totalLength - 1.0
                        , x = 0.0
                    }
            in
            { road | startsAt = newStartNode, endsAt = newEndNode }
    in
    makeStaticVisualEntities context
        preserve3Dspace
        (Array.fromList unrolledRoads)


makeVaryingVisualEntities : ThingsWeNeedForRendering -> Array DrawingRoad -> List (Entity MyCoord)
makeVaryingVisualEntities context roads =
    let
        metresToClipSpace =
            context.scaling.metresToClipSpace

        currentPositionDisc =
            case ( context.currentNode, context.viewingMode ) of
                ( Just road, ThirdPersonView ) ->
                    let
                        ( ( x, y, z ), _ ) =
                            preserve3Dspace road
                    in
                    [ cylinder (Material.color Color.lightOrange) <|
                        Cylinder3d.startingAt
                            (Point3d.meters x y z)
                            (segmentDirection road)
                            { radius = meters <| 10.0 * metresToClipSpace
                            , length = meters <| 1.0 * metresToClipSpace
                            }
                    ]

                _ ->
                    []

        markedNode =
            case ( context.markedNode, context.viewingMode ) of
                ( Just road, ThirdPersonView ) ->
                    let
                        ( ( x, y, z ), _ ) =
                            preserve3Dspace road
                    in
                    [ cone (Material.color Color.purple) <|
                        Cone3d.startingAt
                            (Point3d.meters x y z)
                            (segmentDirection road)
                            { radius = meters <| 6.0 * metresToClipSpace
                            , length = meters <| 3.0 * metresToClipSpace
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
                    2.0 * cos segment.bearing * metresToClipSpace

                kerbY =
                    2.0 * sin segment.bearing * metresToClipSpace

                floatHeight =
                    0.1 * metresToClipSpace

                ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
                    preserve3Dspace segment
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
            { lat = Maybe.withDefault 0.0 <| List.minimum <| List.map .lat tp
            , lon = Maybe.withDefault 0.0 <| List.minimum <| List.map .lon tp
            , ele = Maybe.withDefault 0.0 <| List.minimum <| List.map .ele tp
            , idx = 0
            }

        upperBounds tp =
            { lat = Maybe.withDefault 0.0 <| List.maximum <| List.map .lat tp
            , lon = Maybe.withDefault 0.0 <| List.maximum <| List.map .lon tp
            , ele = Maybe.withDefault 0.0 <| List.maximum <| List.map .ele tp
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
            max (maxs.lat - mins.lat) (maxs.lon - mins.lon)

        metresToClipSpace =
            1 / (0.5 * scalingFactor * metresPerDegreeLatitude)

        elevationToClipSpace e =
            (e - findCentres.ele) * metresToClipSpace
    in
    { mins = mins
    , maxs = maxs
    , centres = findCentres
    , largestDimension = scalingFactor
    , metresToClipSpace = metresToClipSpace
    , seaLevelInClipSpace = elevationToClipSpace 0.0
    }
