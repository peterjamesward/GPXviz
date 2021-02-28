module BendSmoother exposing (..)

import Angle
import Arc2d exposing (Arc2d)
import Geometry101 as G exposing (..)
import Length exposing (Meters, inMeters)
import LineSegment2d
import NodesAndRoads exposing (DrawingRoad)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, xCoordinate, yCoordinate, zCoordinate)
import Polyline2d
import UbiquitousTypes exposing (LocalCoords)


type alias SmoothedBend =
    { nodes : List (Point3d Length.Meters LocalCoords) -- make ourselves work entirely in Meters LocalCoords
    , centre : Point2d Length.Meters LocalCoords
    , radius : Float
    , startIndex : Int -- Lead-in node that is NOT to be replaced
    , endIndex : Int -- ... and lead-out, not to be replaced.
    }


roadToGeometry : DrawingRoad -> G.Road
roadToGeometry road =
    { startAt =
        { x = Length.inMeters <| xCoordinate road.startsAt.xyz
        , y = Length.inMeters <| yCoordinate road.startsAt.xyz
        }
    , endsAt =
        { x = Length.inMeters <| xCoordinate road.endsAt.xyz
        , y = Length.inMeters <| yCoordinate road.endsAt.xyz
        }
    }


pointsToGeometry : Point -> Point -> G.Road
pointsToGeometry p1 p2 =
    { startAt = p1, endsAt = p2 }


toPoint location =
    { x = Length.inMeters <| xCoordinate location
    , y = Length.inMeters <| yCoordinate location
    }


isBefore : Road -> Point -> Bool
isBefore r p =
    antiInterpolate p r.startAt r.endsAt < 0.0


isAfter : Road -> Point -> Bool
isAfter r p =
    antiInterpolate p r.startAt r.endsAt > 1.0


lookForSmoothBendOption : Float -> DrawingRoad -> DrawingRoad -> Maybe SmoothedBend
lookForSmoothBendOption trackPointSpacing roadAB roadCD =
    let
        ( roadIn, roadOut ) =
            ( roadToGeometry roadAB, roadToGeometry roadCD )

        arcFinderGeneral p =
            if isBefore roadIn p && isAfter roadOut p then
                divergentRoadsArc p roadIn roadOut

            else if isAfter roadIn p && isBefore roadOut p then
                convergentRoadsArc p roadIn roadOut

            else
                Nothing

        arc =
            case findIntercept roadIn roadOut of
                Nothing ->
                    parallelFindSemicircle roadIn roadOut

                Just p ->
                    arcFinderGeneral p
    in
    Maybe.map (makeSmoothBend trackPointSpacing roadAB roadCD) arc


withoutElevation : Point3d Length.Meters LocalCoords -> Point2d Length.Meters LocalCoords
withoutElevation p3 =
    let
        { x, y, z } =
            Point3d.toMeters p3
    in
    Point2d.fromMeters { x = x, y = y }


withElevation : Float -> Point2d Length.Meters LocalCoords -> Point3d Length.Meters LocalCoords
withElevation elevation p2 =
    let
        { x, y } =
            Point2d.toMeters p2
    in
    Point3d.fromMeters { x = x, y = y, z = elevation }


makeSmoothBend :
    Float
    -> DrawingRoad
    -> DrawingRoad
    -> Arc2d Meters LocalCoords
    -> SmoothedBend
makeSmoothBend trackPointSpacing roadAB roadCD arc =
    -- Note return list here includes points A and D.
    let
        trueArcLength =
            (abs <| Angle.inRadians <| Arc2d.sweptAngle arc)
                * (Length.inMeters <| Arc2d.radius arc)

        numberPointsOnArc =
            truncate <| trueArcLength / trackPointSpacing

        ( p1, p2 ) =
            -- The first (last) tangent point is also the first (last) point on the arc
            -- so we don't need to pass these as arguments.
            ( Arc2d.startPoint arc |> Point2d.toRecord inMeters
            , Arc2d.endPoint arc |> Point2d.toRecord inMeters
            )

        ( distancePaP1, distanceP2Pc ) =
            ( G.distance (toPoint roadAB.startsAt.xyz) p1
            , G.distance p2 (toPoint roadCD.startsAt.xyz)
            )

        ( fractionalDistanceToTang1, fractionalDistanceToTang2 ) =
            ( distancePaP1 / roadAB.length
            , distanceP2Pc / roadCD.length
            )

        tang1 =
            Point3d.interpolateFrom
                roadAB.startsAt.xyz
                roadAB.endsAt.xyz
                fractionalDistanceToTang1

        tang2 =
            Point3d.interpolateFrom
                roadCD.startsAt.xyz
                roadCD.endsAt.xyz
                fractionalDistanceToTang2

        ( elevationArcStart, elevationArcEnd ) =
            ( Length.inMeters <| zCoordinate tang1
            , Length.inMeters <| zCoordinate tang2
            )

        segments =
            Arc2d.segments (numberPointsOnArc - 1) arc
                |> Polyline2d.segments

        eleIncrement =
            (elevationArcEnd - elevationArcStart) / toFloat numberPointsOnArc

        elevate point2d i =
            withElevation
                (elevationArcStart + toFloat i * eleIncrement)
                point2d

        newArcPoints =
            List.map2
                elevate
                (List.map LineSegment2d.startPoint (List.take 1 segments))
                [ 0 ]
                ++ List.map2
                    elevate
                    (List.map LineSegment2d.endPoint segments)
                    (List.range 1 (numberPointsOnArc + 10))
    in
    { nodes =
        roadAB.startsAt.xyz
            :: newArcPoints
            ++ [ roadCD.endsAt.xyz ]
    , centre = Arc2d.centerPoint arc
    , radius = inMeters <| Arc2d.radius arc
    , startIndex = roadAB.index
    , endIndex = roadCD.index + 1
    }


divergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
divergentRoadsArc p r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt ), ( r2.startAt, r2.endsAt ) )

        ( midAB, midCD ) =
            ( interpolateLine 0.5 pa pb, interpolateLine 0.5 pc pd )

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( firstTangentPoint, secondTangentPoint ) =
            -- For divergence, choose midpoint farthest from interesct p.
            if distance p midAB >= distance p midCD then
                ( midAB, pointAlongRoad (pointsToGeometry p pc) (distance p midAB) )

            else
                ( pointAlongRoad (pointsToGeometry p pb) (distance p midCD), midCD )

        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation firstTangentPoint, linePerpendicularTo r2Equation secondTangentPoint )

        circleCenter =
            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

        findArc centre =
            let
                radius =
                    distance centre firstTangentPoint

                bisectorAsRoad =
                    { startAt = p, endsAt = centre }

                midArcPoint =
                    pointAlongRoad
                        bisectorAsRoad
                        (radius + distance p centre)
            in
            Arc2d.throughPoints
                (Point2d.meters firstTangentPoint.x firstTangentPoint.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters secondTangentPoint.x secondTangentPoint.y)
    in
    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter


convergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
convergentRoadsArc p r1 r2 =
    --TODO: These arcs don't seem to make tangent contact with the lines.
    -- PERHAPS we should not work in LocalCoords, not GPXCoords.
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt )
            , ( r2.startAt, r2.endsAt )
            )

        ( midAB, midCD ) =
            ( interpolateLine 0.5 pa pb, interpolateLine 0.5 pc pd )

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( firstTangentPoint, secondTangentPoint ) =
            if distance p midAB <= distance p midCD then
                ( midAB, pointAlongRoad (pointsToGeometry p pd) (distance p midAB) )

            else
                ( pointAlongRoad (pointsToGeometry p pa) (distance p midCD), midCD )

        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation firstTangentPoint, linePerpendicularTo r2Equation secondTangentPoint )

        circleCenter =
            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

        findArc centre =
            let
                radius =
                    distance centre firstTangentPoint

                bisectorAsRoad =
                    { startAt = centre, endsAt = p }

                midArcPoint =
                    pointAlongRoad bisectorAsRoad radius
            in
            Arc2d.throughPoints
                (Point2d.meters firstTangentPoint.x firstTangentPoint.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters secondTangentPoint.x secondTangentPoint.y)
    in
    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter


parallelFindSemicircle : Road -> Road -> Maybe (Arc2d Meters LocalCoords)
parallelFindSemicircle r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt )
            , ( r2.startAt, r2.endsAt )
            )

        ( midAB, midBC ) =
            ( interpolateLine 0.5 pa pb
            , interpolateLine 0.5 pb pc
            )

        ( midCD, midDA ) =
            ( interpolateLine 0.5 pc pd
            , interpolateLine 0.5 pd pa
            )

        middle =
            -- As lines are parallel, we can use this as the circle centre.
            interpolateLine 0.5 midBC midDA

        centreLine =
            { startAt = middle, endsAt = midBC }

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( radiusToFirstTangentPoint, radiusToSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation middle, linePerpendicularTo r2Equation middle )

        ( firstTangentPoint, secondTangentPoint ) =
            ( lineIntersection r1Equation radiusToFirstTangentPoint
            , lineIntersection r2Equation radiusToSecondTangentPoint
            )
    in
    case ( firstTangentPoint, secondTangentPoint ) of
        ( Just t1, Just t2 ) ->
            let
                radius =
                    distance middle t1

                midArcPoint =
                    pointAlongRoad centreLine radius
            in
            Arc2d.throughPoints
                (Point2d.meters t1.x t1.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters t2.x t2.y)

        _ ->
            Nothing
