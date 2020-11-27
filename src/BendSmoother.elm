module BendSmoother exposing (..)

import Arc2d exposing (Arc2d, throughPoints)
import Geometry101 as G exposing (..)
import Length exposing (Meters, inMeters)
import LineSegment2d
import NodesAndRoads exposing (MyCoord)
import Point2d exposing (xCoordinate, yCoordinate)
import Polyline2d
import Spherical exposing (metresPerDegreeLatitude)
import TrackPoint exposing (TrackPoint)


type alias SmoothedBend =
    { trackPoints : List TrackPoint
    , centre : ( Float, Float )
    , radius : Float
    , startIndex : Int -- First trackpoint to be replaced
    , endIndex : Int -- and last
    }


roadToGeometry : TrackPoint -> TrackPoint -> G.Road
roadToGeometry startsAt endsAt =
    { startAt = { x = startsAt.lon, y = startsAt.lat }
    , endsAt = { x = endsAt.lon, y = endsAt.lat }
    }


toTrackPoint : Point -> TrackPoint
toTrackPoint p =
    -- NOTE caller has to fix up the missing fields!
    { lat = p.y
    , lon = p.x
    , ele = 0.0
    , idx = 0
    }


toPoint tp =
    { x = tp.lon, y = tp.lat }


isBefore : Road -> Point -> Bool
isBefore r p =
    antiInterpolate p r.startAt r.endsAt < 0.0


isAfter : Road -> Point -> Bool
isAfter r p =
    antiInterpolate p r.startAt r.endsAt > 1.0


bendIncircle : Int -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> Maybe SmoothedBend
bendIncircle numSegments pa pb pc pd =
    let
        ( roadIn, roadOut ) =
            ( roadToGeometry pa pb, roadToGeometry pc pd )

        arcFinderGeneral p r1 r2 =
            if isBefore r1 p && isAfter r2 p then
                divergentRoadsArc p r1 r2

            else
                convergentRoadsArc p r1 r2

        arc =
            case findIntercept roadIn roadOut of
                Nothing ->
                    parallelFindSemicircle roadIn roadOut

                Just p ->
                    arcFinderGeneral p roadIn roadOut
    in
    Maybe.map (makeSmoothBend numSegments pa pb pc pd) arc


makeSmoothBend :
    Int
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> Arc2d Meters MyCoord
    -> SmoothedBend
makeSmoothBend numSegments pa pb pc pd arc =
    let
        ( p1, p2 ) =
            -- The first (last) tangent point is also the first (last) point on the arc
            -- so we don't need to pass these as arguments.
            ( Arc2d.startPoint arc |> Point2d.toRecord inMeters
            , Arc2d.endPoint arc |> Point2d.toRecord inMeters
            )

        t1 =
            -- Elevations of tangent points are interpolations
            -- within the segments that contain them.
            { lat = p1.y
            , lon = p1.x
            , ele =
                interpolateScalar
                    (min 1.0 (whatFraction p1 (toPoint pa) (toPoint pb)))
                    pa.ele
                    pb.ele
            }

        t2 =
            -- Elevations of tangent points are interpolations
            -- within the segments that contain them.
            { lat = p2.y
            , lon = p2.x
            , ele =
                interpolateScalar
                    (min 1.0 (whatFraction p2 (toPoint pd) (toPoint pc)))
                    pd.ele
                    pc.ele
            , idx = 0
            }

        eleIncrement =
            (t2.ele - t1.ele) / toFloat numSegments

        segments =
            Arc2d.segments numSegments arc
                |> Polyline2d.segments

        newTrackPoints =
            List.map2
                (\seg i ->
                    let
                        p0 =
                            LineSegment2d.startPoint seg
                    in
                    { lon = xCoordinate p0 |> Length.inMeters
                    , lat = yCoordinate p0 |> Length.inMeters
                    , ele = t1.ele + toFloat i * eleIncrement
                    , idx = 0
                    }
                )
                segments
                (List.range 0 numSegments)

        asPair p2d =
            let
                asRecord =
                    Point2d.toRecord Length.inMeters p2d
            in
            ( asRecord.x, asRecord.y )
    in
    { trackPoints =
        pa
            :: newTrackPoints
            ++ [ t2, pd ]
    , centre = asPair <| Arc2d.centerPoint arc
    , radius = metresPerDegreeLatitude * (inMeters <| Arc2d.radius arc)
    , startIndex = pa.idx
    , endIndex = pd.idx
    }


divergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters MyCoord)
divergentRoadsArc p r1 r2 =
    -- In this case we prefer to find a more-than-semi-circle that
    -- joins ends B and C, preserving the length of the road segments.
    -- (Simply because that's how I see this situation.)
    -- We will be using the incircle of PBC, but only to give us the
    -- bisector of angle BPC. The place where the normal from AB or DC
    -- crosses this bisector (whichever is furthest from P) becomes our
    -- circle centre. A point on the far side becomes our mid arc point.
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt ), ( r2.startAt, r2.endsAt ) )

        ( farthestEndPoint, dominantRoad, otherRoad ) =
            if distance p pb >= distance p pc then
                ( pb, r1, r2 )

            else
                ( pc, r2, r1 )

        maybeCircle =
            G.findIncircleFromTwoRoads r1 r2
    in
    case maybeCircle of
        Just circle ->
            let
                bisector =
                    lineEquationFromTwoPoints p circle.centre

                dominantRoadAsLine =
                    lineEquationFromTwoPoints dominantRoad.startAt dominantRoad.endsAt

                perpFromDominantRoad =
                    linePerpendicularTo dominantRoadAsLine farthestEndPoint

                maybeCentre =
                    lineIntersection bisector perpFromDominantRoad
            in
            case maybeCentre of
                Just centre ->
                    let
                        otherRoadAsLine =
                            lineEquationFromTwoPoints otherRoad.startAt otherRoad.endsAt

                        perpToOtherRoad =
                            linePerpendicularTo otherRoadAsLine centre

                        otherTangentPoint =
                            lineIntersection perpToOtherRoad otherRoadAsLine

                        radius =
                            -- Need this to find the mid point
                            distance centre farthestEndPoint

                        bisectorAsRoad =
                            { startAt = p, endsAt = centre }

                        distanceToCentre =
                            distance p centre

                        midArcPoint =
                            pointAlongRoad bisectorAsRoad (distanceToCentre + radius)

                        ( arcStart, arcFinish ) =
                            if dominantRoad == r1 then
                                ( Just r1.endsAt, otherTangentPoint )

                            else
                                ( otherTangentPoint, Just r2.startAt )
                    in
                    case ( arcStart, arcFinish ) of
                        ( Just p1, Just p2 ) ->
                            Arc2d.throughPoints
                                (Point2d.meters p1.x p1.y)
                                (Point2d.meters midArcPoint.x midArcPoint.y)
                                (Point2d.meters p2.x p2.y)

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


parallelFindSemicircle : Road -> Road -> Maybe (Arc2d Meters MyCoord)
parallelFindSemicircle r1 r2 =
    Nothing


convergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters MyCoord)
convergentRoadsArc p r1 r2 =
    let
        maybeCircle =
            G.findIncircleFromTwoRoads r1 r2
    in
    case maybeCircle of
        Just circle ->
            weHaveAnIncircle p r1 r2 circle

        Nothing ->
            Nothing


weHaveAnIncircle : Point -> Road -> Road -> Circle -> Maybe (Arc2d Meters MyCoord)
weHaveAnIncircle p r1 r2 circle =
    let
        ( entryPoint, exitPoint ) =
            ( G.findTangentPoint r1 circle
            , G.findTangentPoint r2 circle
            )
    in
    case ( entryPoint, exitPoint ) of
        ( Just p1, Just p2 ) ->
            -- Solved the tangent points.
            -- We'll use Arc2d to find our approximation.
            -- First, we find an intermediate point on the circle,
            -- intersecting with the line connecting the centre (known) to
            -- the intersection of the tangents (p).
            let
                distCentreToP =
                    distance circle.centre p

                proportion =
                    circle.radius / distCentreToP

                midArcPoint =
                    interpolateLine proportion circle.centre p
            in
            Arc2d.throughPoints
                (Point2d.meters p1.x p1.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters p2.x p2.y)

        _ ->
            Nothing
