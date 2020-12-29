module BendSmoother exposing (..)

import Angle
import Arc2d exposing (Arc2d)
import Geometry101 as G exposing (..)
import Length exposing (Meters, inMeters)
import LineSegment2d
import NodesAndRoads exposing (LocalCoords)
import Point2d exposing (xCoordinate, yCoordinate)
import Polyline2d
import Spherical exposing (metresPerDegree)
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


toPoint tp =
    { x = tp.lon, y = tp.lat }


isBefore : Road -> Point -> Bool
isBefore r p =
    antiInterpolate p r.startAt r.endsAt < 0.0


isAfter : Road -> Point -> Bool
isAfter r p =
    antiInterpolate p r.startAt r.endsAt > 1.0


bendIncircle : Float -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> Maybe SmoothedBend
bendIncircle trackPointSpacing pa pb pc pd =
    let
        ( roadIn, roadOut ) =
            ( roadToGeometry pa pb, roadToGeometry pc pd )

        arcFinderGeneral p r1 r2 =
            if isBefore r1 p && isAfter r2 p then
                divergentRoadsArc p r1 r2

            else
                newConvergentRoadsArc p r1 r2

        arc =
            case findIntercept roadIn roadOut of
                Nothing ->
                    parallelFindSemicircle roadIn roadOut

                Just p ->
                    arcFinderGeneral p roadIn roadOut
    in
    Maybe.map (makeSmoothBend trackPointSpacing pa pb pc pd) arc


makeSmoothBend :
    Float
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> Arc2d Meters LocalCoords
    -> SmoothedBend
makeSmoothBend trackPointSpacing pa pb pc pd arc =
    let
        trueArcLength =
            (abs <| Angle.inRadians <| Arc2d.sweptAngle arc)
                * metresPerDegree * (Length.inMeters <| Arc2d.radius arc)

        numberPointsOnArc =
            truncate <| trueArcLength / trackPointSpacing

        ( p1, p2 ) =
            -- The first (last) tangent point is also the first (last) point on the arc
            -- so we don't need to pass these as arguments.
            ( Arc2d.startPoint arc |> Point2d.toRecord inMeters
            , Arc2d.endPoint arc |> Point2d.toRecord inMeters
            )

        netElevationChange =
            -- Spread the elevation change uniformly by road length.
            pd.ele - pa.ele

        ( distancePaP1, distanceP2Pd ) =
            ( G.distance (toPoint pa) p1, G.distance p2 (toPoint pd) )

        t1 =
            -- Elevations of tangent points are interpolations
            -- within the segments that contain them.
            { lat = p1.y
            , lon = p1.x
            , ele =
                interpolateScalar
                    (distancePaP1 / newLength)
                    pa.ele
                    pd.ele
            , idx = 0
            }

        t2 =
            -- Elevations of tangent points are interpolations
            -- within the segments that contain them.
            { lat = p2.y
            , lon = p2.x
            , ele =
                interpolateScalar
                    ((distancePaP1 + arcLength) / newLength)
                    pa.ele
                    pd.ele
            , idx = 0
            }

        segments =
            Arc2d.segments (numberPointsOnArc + 1) arc
                |> Polyline2d.segments

        arcLength =
            List.sum <| List.map (LineSegment2d.length >> Length.inMeters) segments

        newLength =
            distancePaP1 + arcLength + distanceP2Pd

        eleIncrement =
            (t2.ele - t1.ele) / toFloat (numberPointsOnArc + 1)

        newTrackPoints =
            List.drop 1 <|
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
                    (List.range 0 (numberPointsOnArc - 1))

        asPair p2d =
            let
                asRecord =
                    Point2d.toRecord Length.inMeters p2d
            in
            ( asRecord.x, asRecord.y )
    in
    { trackPoints =
        [ pa ]
            ++ newTrackPoints
            ++ [ pd ]
    , centre = asPair <| Arc2d.centerPoint arc
    , radius = metresPerDegree * (inMeters <| Arc2d.radius arc)
    , startIndex = pa.idx
    , endIndex = pd.idx
    }


divergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
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


newConvergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
newConvergentRoadsArc p r1 r2 =
    -- With minor changes the divergent arc should be a model for a simple
    -- convergent case, not using the Incircle (and hence larger radii generally).
    -- p (the intersect) will be the intersection of the extended pa-pb and pd-pc lines.
    -- The 'dominant' road will be determined by which  of B or C is nearest P.
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt ), ( r2.startAt, r2.endsAt ) )

        ( endPointNearestIntersect, dominantRoad, otherRoad ) =
            if distance p pa <= distance p pd then
                ( pa, r1, r2 )

            else
                ( pd, r2, r1 )

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
                    linePerpendicularTo dominantRoadAsLine endPointNearestIntersect

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
                            distance centre endPointNearestIntersect

                        bisectorAsRoad =
                            { startAt = p, endsAt = centre }

                        distanceToCentre =
                            distance p centre

                        midArcPoint =
                            pointAlongRoad bisectorAsRoad (distanceToCentre - radius)

                        ( arcStart, arcFinish ) =
                            if dominantRoad == r1 then
                                ( Just r1.startAt, otherTangentPoint )

                            else
                                ( otherTangentPoint, Just r2.endsAt )
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


parallelFindSemicircle : Road -> Road -> Maybe (Arc2d Meters LocalCoords)
parallelFindSemicircle r1 r2 =
    Nothing


convergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
convergentRoadsArc p r1 r2 =
    let
        maybeCircle =
            G.findIncircleFromTwoRoads r1 r2
    in
    case maybeCircle of
        Just circle ->
            findArcFromIncircle p r1 r2 circle

        Nothing ->
            Nothing


findArcFromIncircle : Point -> Road -> Road -> Circle -> Maybe (Arc2d Meters LocalCoords)
findArcFromIncircle p r1 r2 circle =
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
