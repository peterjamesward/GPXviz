module NodesAndRoads exposing (..)

import ScalingInfo exposing (ScalingInfo)
import Spherical exposing (metresPerDegreeLatitude)
import TrackPoint exposing (TrackPoint)


type MyCoord
    = SomeCoord


type alias DrawingNode =
    -- We draw in a rectangular space using metre units.
    --TODO: Convert to Point3d Meters
    { trackPoint : TrackPoint
    , northOffset : Float -- metres from bottom edge of bounding box
    , eastOffset : Float -- metres from left edge of bounding box
    , vertOffset : Float -- metres from base of bounding box
    , x : Float -- east offset converted
    , y : Float -- north, ditto
    , z : Float -- vert, ditto
    }


type alias DrawingRoad =
    --TODO: Use LineSegment3d ??
    { startsAt : DrawingNode
    , endsAt : DrawingNode
    , length : Float
    , bearing : Float
    , gradient : Float -- radians
    , startDistance : Float
    , endDistance : Float
    , index : Int
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


deriveNodes : ScalingInfo -> List TrackPoint -> List DrawingNode
deriveNodes scale tps =
    let
        findCentres =
            scale.centres

        mins =
            scale.mins

        prepareDrawingNode tp =
            { trackPoint = tp
            , northOffset = (tp.lat - mins.lat) * metresPerDegreeLatitude
            , eastOffset = (tp.lon - mins.lon) * metresPerDegreeLatitude * cos tp.lat
            , vertOffset = tp.ele - mins.ele
            , x = (tp.lon - findCentres.lon) * metresPerDegreeLatitude
            , y = (tp.lat - findCentres.lat) * metresPerDegreeLatitude
            , z = tp.ele
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
                    Spherical.range ( degrees node2.trackPoint.lat, degrees node2.trackPoint.lon )
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
            }

        roadSegments =
            List.map2 roadSegment
                drawingNodes
                (List.drop 1 drawingNodes)

        ( _, _, withAccumulations ) =
            List.foldl
                (\road ( idx, dist, done ) ->
                    ( idx + 1
                    , dist + road.length
                    , { road
                        | startDistance = dist
                        , endDistance = dist + road.length
                        , index = idx
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


roadsForProfileView : List DrawingRoad -> List DrawingRoad
roadsForProfileView roads =
    -- Don't try to be clever. Be pragmatic.
    let
        unrolledRoads : List DrawingRoad
        unrolledRoads =
            List.map unrollRoad roads

        unrollRoad : DrawingRoad -> DrawingRoad
        unrollRoad road =
            let
                startNode =
                    road.startsAt

                endNode =
                    road.endsAt

                newStartNode =
                    { startNode
                        | y = 2.0 * road.startDistance / 10.0 -- compress horizontal scale.
                        , x = 0.0
                    }

                newEndNode =
                    { endNode
                        | y = 2.0 * road.endDistance / 10.0 -- compress horizontal scale.
                        , x = 0.0
                    }
            in
            { road | startsAt = newStartNode, endsAt = newEndNode, bearing = 0.0 }
    in
    unrolledRoads
