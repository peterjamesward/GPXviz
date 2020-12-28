module NodesAndRoads exposing (..)

--import ScalingInfo exposing (ScalingInfo)

import Area
import BoundingBox3d exposing (BoundingBox3d)
import Element exposing (alignTop, centerX, column, none, padding, row, spacing, text)
import Length
import Point3d exposing (Point3d)
import Quantity
import Spherical exposing (metresPerDegree)
import Stat
import TrackPoint exposing (GPXCoords, TrackPoint)
import Triangle3d
import Utils exposing (bearingToDisplayDegrees, showDecimal2, showDecimal6)


type LocalCoords
    = LocalCoords


type alias ScalingInfo =
    { nodeBox : BoundingBox3d Length.Meters LocalCoords
    , trackPointBox : BoundingBox3d Length.Meters GPXCoords
    }


type alias DrawingNode =
    -- Track point with Web Mercator projection.
    { trackPoint : TrackPoint
    , location : Point3d.Point3d Length.Meters LocalCoords
    }


type alias DrawingRoad =
    -- Everything about the gap between track points, aka road segment.
    { startsAt : DrawingNode
    , endsAt : DrawingNode
    , profileStartsAt : DrawingNode -- x coord is metres from track start.
    , profileEndsAt : DrawingNode
    , length : Float
    , bearing : Float
    , gradient : Float -- (percent == rise/run == tangent of slope, if you want to know)
    , startDistance : Float
    , endDistance : Float
    , index : Int -- N.B. Will be same index as starting track point.
    }


type alias SummaryData =
    { highestMetres : Float
    , lowestMetres : Float
    , trackLength : Float
    , climbingDistance : Float
    , descendingDistance : Float
    , totalClimbing : Float
    , totalDescending : Float
    }


deriveTrackPointBox : List TrackPoint -> BoundingBox3d Length.Meters LocalCoords
deriveTrackPointBox tps =
    Maybe.withDefault
        (BoundingBox3d.singleton <| Point3d.meters 0.0 0.0 0.0)
    <|
        BoundingBox3d.hullN <|
            List.map (\tp -> Point3d.meters tp.lon tp.lat tp.ele)
                tps


deriveNodes : BoundingBox3d Length.Meters GPXCoords -> List TrackPoint -> List DrawingNode
deriveNodes box tps =
    let
        ( midLon, midLat, _ ) =
            Point3d.toTuple Length.inMeters <|
                BoundingBox3d.centerPoint box

        {- double[] WGS84toGoogleBing(double lon, double lat) {
             double x = lon * 20037508.34 / 180;
             double y = Math.Log(Math.Tan((90 + lat) * Math.PI / 360)) / (Math.PI / 180);
             y = y * 20037508.34 / 180;
             return new double[] {x, y};
           }
        -}
        mercatorX lon =
            lon * 20037508.34 / 180

        mercatorY lat =
            (20037508.34 / pi) * logBase e (tan (degrees (45 + lat / 2.0)))

        ( midX, midY ) =
            ( mercatorX midLon, mercatorY midLat )

        prepareDrawingNode tp =
            { trackPoint = tp
            , location =
                Point3d.meters
                    (mercatorX tp.lon - midX)
                    (mercatorY tp.lat - midY)
                    tp.ele
            }
    in
    List.map prepareDrawingNode tps


deriveRoads : List DrawingNode -> List DrawingRoad
deriveRoads drawingNodes =
    let
        roadSegment node1 node2 =
            let
                zDifference =
                    node2.trackPoint.ele - node1.trackPoint.ele

                earthDistance =
                    -- Great circle distance (!) ignoring elevation difference
                    Spherical.range
                        ( degrees node2.trackPoint.lat, degrees node2.trackPoint.lon )
                        ( degrees node1.trackPoint.lat, degrees node1.trackPoint.lon )
            in
            { startsAt = node1
            , endsAt = node2
            , length = earthDistance
            , bearing =
                Spherical.findBearingToTarget
                    ( degrees node1.trackPoint.lat, degrees node1.trackPoint.lon )
                    ( degrees node2.trackPoint.lat, degrees node2.trackPoint.lon )
            , gradient =
                if earthDistance > 0 then
                    100.0 * (zDifference / earthDistance)

                else
                    0.0
            , startDistance = 0.0
            , endDistance = 0.0
            , index = 0
            , profileStartsAt = node1
            , profileEndsAt = node2 -- These will be replaced.
            }

        roadSegments =
            List.map2 roadSegment
                drawingNodes
                (List.drop 1 drawingNodes)

        ( _, _, withAccumulations ) =
            List.foldl
                (\road ( idx, dist, done ) ->
                    let
                        startNode =
                            road.startsAt

                        endNode =
                            road.endsAt

                        profileStartAt =
                            { startNode
                                | location =
                                    Point3d.xyz
                                        (Length.meters 0.0)
                                        (Length.meters <| dist / 5.0)
                                        (Point3d.zCoordinate startNode.location)
                            }

                        profileEndAt =
                            { endNode
                                | location =
                                    --TODO: The divide by 5.0 should not be happening here.
                                    Point3d.xyz
                                        (Length.meters 0.0)
                                        (Length.meters <| (dist + road.length) / 5.0)
                                        (Point3d.zCoordinate endNode.location)
                            }
                    in
                    ( idx + 1
                    , dist + road.length
                    , { road
                        | startDistance = dist
                        , endDistance = dist + road.length
                        , index = idx
                        , profileStartsAt = profileStartAt
                        , profileEndsAt = profileEndAt
                      }
                        :: done
                    )
                )
                ( 0, 0.0, [] )
                roadSegments
    in
    List.reverse withAccumulations


deriveSummary : List DrawingRoad -> SummaryData
deriveSummary roadSegments =
    let
        accumulateInfo segment summary =
            { trackLength = summary.trackLength + segment.length
            , highestMetres =
                max summary.highestMetres <|
                    max segment.startsAt.trackPoint.ele segment.endsAt.trackPoint.ele
            , lowestMetres =
                min summary.lowestMetres <|
                    min segment.startsAt.trackPoint.ele segment.endsAt.trackPoint.ele
            , climbingDistance =
                if segment.gradient > 0 then
                    summary.climbingDistance + segment.length

                else
                    summary.climbingDistance
            , descendingDistance =
                if segment.gradient < 0 then
                    summary.climbingDistance + segment.length

                else
                    summary.climbingDistance
            , totalClimbing =
                if segment.gradient > 0 then
                    summary.totalClimbing + segment.endsAt.trackPoint.ele - segment.startsAt.trackPoint.ele

                else
                    summary.totalClimbing
            , totalDescending =
                if segment.gradient < 0 then
                    summary.totalClimbing - segment.endsAt.trackPoint.ele + segment.startsAt.trackPoint.ele

                else
                    summary.totalClimbing
            }
    in
    List.foldl accumulateInfo
        { trackLength = 0.0
        , highestMetres = -9999.9
        , lowestMetres = 9999.9
        , climbingDistance = 0.0
        , descendingDistance = 0.0
        , totalClimbing = 0.0
        , totalDescending = 0.0
        }
        roadSegments


summaryData maybeRoad =
    case maybeRoad of
        Just road ->
            column [ centerX ]
                [ row [ padding 20, centerX, spacing 10 ]
                    [ column [ spacing 10 ]
                        [ text "Start point index "
                        , text "Length "
                        ]
                    , column [ spacing 10 ]
                        [ text <| String.fromInt road.index
                        , text <| showDecimal2 road.length
                        ]
                    , column [ spacing 10 ]
                        [ text "Gradient "
                        , text "Bearing "
                        ]
                    , column [ spacing 10 ]
                        [ text <| showDecimal2 road.gradient
                        , text <| bearingToDisplayDegrees road.bearing
                        ]
                    ]
                , row [ padding 10, centerX, alignTop, spacing 10 ]
                    [ column [ spacing 10 ]
                        [ text "   "
                        , text "Latitude "
                        , text "Longitude "
                        , text "Elevation "
                        , text "Distance "
                        ]
                    , column [ spacing 10 ]
                        [ text "At start"
                        , text <| showDecimal6 road.startsAt.trackPoint.lat
                        , text <| showDecimal6 road.startsAt.trackPoint.lon
                        , text <| showDecimal2 road.startsAt.trackPoint.ele
                        , text <| showDecimal2 road.startDistance
                        ]
                    , column [ spacing 10 ]
                        [ text "At end"
                        , text <| showDecimal6 road.endsAt.trackPoint.lat
                        , text <| showDecimal6 road.endsAt.trackPoint.lon
                        , text <| showDecimal2 road.endsAt.trackPoint.ele
                        , text <| showDecimal2 road.endDistance
                        ]
                    ]
                ]

        Nothing ->
            none


type alias MinimaAccumulator =
    { localMinimum : Maybe ( DrawingNode, Float )
    , collected : List ( DrawingNode, Float )
    }


metricFilteredNodes : List DrawingNode -> List DrawingNode
metricFilteredNodes nodes =
    -- Tool to reduce excessive track points. (Jarle Steffenson).
    -- Would it not be easier just to sort all nodes by increasing cost and pick those
    -- avoiding picking a "run"? This last comment is definitely important!!
    let
        nodesWithMetrics =
            List.map3
                (\a b c -> ( b, costMetric a b c ))
                nodes
                (List.drop 1 nodes)
                (List.drop 2 nodes)

        costMetric : DrawingNode -> DrawingNode -> DrawingNode -> Float
        costMetric a b c =
            -- Let's see if area is a good metric.
            -- Maybe just adding bearing and gradient changes is better. Test it.
            Area.inSquareMeters <|
                Triangle3d.area <|
                    Triangle3d.fromVertices
                        ( a.location, b.location, c.location )

        numberOfNodes =
            List.length nodes

        sortedNodes =
            List.sortBy Tuple.second nodesWithMetrics

        fraction =
            0.1

        -- fraction to attempt to remove
        thresholdMetric =
            case List.head <| List.drop (truncate (fraction * toFloat numberOfNodes)) sortedNodes of
                Just ( _, metric ) ->
                    metric

                Nothing ->
                    0.0

        retainedNodes =
            List.map Tuple.first <|
                List.filter (\( node, metric ) -> metric > thresholdMetric) nodesWithMetrics
    in
    List.take 1 nodes
        ++ retainedNodes
        ++ (List.take 1 <| List.reverse nodes)
