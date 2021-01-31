module Filters exposing (applyWeightedAverageFilter, bezierSplines)

import Array
import CubicSpline3d exposing (CubicSpline3d)
import Length
import LineSegment3d exposing (LineSegment3d)
import Loop exposing (..)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import TrackPoint exposing (GPXCoords, TrackPoint, pointAsTrackPoint, pointFromTrackpoint)
import Triangle3d exposing (Triangle3d)
import Vector3d


type alias ControlPoint =
    Point3d Length.Meters GPXCoords


type alias FilterFunction =
    (TrackPoint -> Float)
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> Float


applyWeightedAverageFilter :
    ( Int, Int )
    -> Float
    -> Loopiness
    -> List TrackPoint
    -> List TrackPoint
applyWeightedAverageFilter ( start, finish ) filterBias loopiness points =
    let
        firstPoint =
            List.take 1 points

        numPoints =
            List.length points

        loopedPoints =
            points ++ List.drop 1 points

        filteredLoop =
            List.map3
                (weightedAverage filterBias)
                (List.drop (numPoints - 1) loopedPoints)
                points
                (List.drop 1 loopedPoints)

        filtered =
            List.map3
                (weightedAverage filterBias)
                (firstPoint ++ points)
                points
                (List.drop 1 loopedPoints)

        withinRange =
            Array.fromList
                >> Array.slice (start + 1) finish
                >> Array.toList

        ( fixedFirst, fixedLast ) =
            ( List.take (start + 1) points, List.drop finish points )
    in
    if start == finish && loopiness == IsALoop then
        filteredLoop

    else
        fixedFirst ++ withinRange filtered ++ fixedLast


weightedAverage :
    Float
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
weightedAverage bias p0 p1 p2 =
    { lon = (1.0 - bias) * p1.lon + bias * (p0.lon + p1.lon + p2.lon) / 3.0
    , lat = (1.0 - bias) * p1.lat + bias * (p0.lat + p1.lat + p2.lat) / 3.0
    , ele = (1.0 - bias) * p1.ele + bias * (p0.ele + p1.ele + p2.ele) / 3.0
    , idx = p1.idx
    }


bezierSplines : Bool -> List TrackPoint -> List TrackPoint
bezierSplines isLoop points =
    let
        _ =
            Debug.log "Look, ma! Splines!" asTrackPointsAgain

        tension =
            -- This seems to be empirical measure of smoothness.
            0.5

        tolerance =
            -- As is this, given we're in weird coordinate land!
            0.01

        firstPoint =
            List.take 1 points

        lastPoint =
            -- Inefficient but avoids Maybe.
            List.take 1 <| List.reverse points

        makeTriangleForTrackPoint :
            TrackPoint
            -> TrackPoint
            -> TrackPoint
            -> Triangle3d Length.Meters GPXCoords
        makeTriangleForTrackPoint pa pb pc =
            -- Note order of points here is shaped by the way that Triangle3d returns edges.
            -- We're really interested in pa-pc, since that determines pb's control points.
            Triangle3d.from
                (pointFromTrackpoint pb)
                (pointFromTrackpoint pa)
                (pointFromTrackpoint pc)

        makeTriangles : List (Triangle3d Length.Meters GPXCoords)
        makeTriangles =
            let
                shiftedBack =
                    if isLoop then
                        lastPoint ++ points
                    else
                        firstPoint ++ points
                shiftedForwards =
                    if isLoop then
                        (List.drop 1 points ++ firstPoint)
                    else
                        (List.drop 1 points ++ lastPoint)
            in
            List.map3
                makeTriangleForTrackPoint
                shiftedBack
                points
                shiftedForwards

        controlPointsFromTriangle :
            Triangle3d Length.Meters GPXCoords
            -> ( ControlPoint, ControlPoint, ControlPoint )
        controlPointsFromTriangle triangle =
            let
                ( b, _, _ ) =
                    Triangle3d.vertices triangle

                ( entryEdge, oppositeEdge, exitEdge ) =
                    Triangle3d.edges triangle

                ( ab, ac, bc ) =
                    ( Length.inMeters <| LineSegment3d.length entryEdge
                    , Length.inMeters <| LineSegment3d.length oppositeEdge
                    , Length.inMeters <| LineSegment3d.length exitEdge
                    )

                ( entryFactor, exitFactor ) =
                    ( -1.0 * tension * ab / (ab + bc)
                    , tension * bc / (ab + bc)
                    )

                controlPointVector =
                    Vector3d.from
                        (LineSegment3d.startPoint oppositeEdge)
                        (LineSegment3d.endPoint oppositeEdge)

                ( entryScaleVector, exitScalevector ) =
                    ( Vector3d.scaleBy entryFactor controlPointVector
                    , Vector3d.scaleBy exitFactor controlPointVector
                    )

                ( entryPoint, exitPoint ) =
                    ( Point3d.translateBy entryScaleVector b
                    , Point3d.translateBy exitScalevector b
                    )
            in
            ( entryPoint, b, exitPoint )

        makeControlPoints : List ( ControlPoint, ControlPoint, ControlPoint )
        makeControlPoints =
            List.map
                controlPointsFromTriangle
                makeTriangles

        makeSpline :
            ( ControlPoint, ControlPoint, ControlPoint )
            -> ( ControlPoint, ControlPoint, ControlPoint )
            -> CubicSpline3d Length.Meters GPXCoords
        makeSpline ( _, start, control1 ) ( control2, end, _ ) =
            CubicSpline3d.fromControlPoints
                start
                control1
                control2
                end

        makeSplines : List (CubicSpline3d Length.Meters GPXCoords)
        makeSplines =
            List.map2
                makeSpline
                makeControlPoints
                (List.drop 1 makeControlPoints)

        asPolylines : List (Polyline3d Length.Meters GPXCoords)
        asPolylines =
            List.map
                (CubicSpline3d.approximate (Length.meters tolerance))
                makeSplines

        asSegments : List (LineSegment3d Length.Meters GPXCoords)
        asSegments =
            List.concatMap
                Polyline3d.segments
                asPolylines

        asTrackPointsAgain : List TrackPoint
        asTrackPointsAgain =
            List.map
                (LineSegment3d.startPoint >> pointAsTrackPoint)
                (List.take 1 asSegments) ++
            List.map
                (LineSegment3d.endPoint >> pointAsTrackPoint)
                asSegments
    in
    asTrackPointsAgain
