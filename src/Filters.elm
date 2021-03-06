module Filters exposing (applyWeightedAverageFilter, bezierSplines)

import Array
import CubicSpline3d exposing (CubicSpline3d)
import Length
import LineSegment3d exposing (LineSegment3d)
import Loop exposing (..)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import TrackPoint exposing (GPXCoords, TrackPoint, toGPXcoords)
import Triangle3d exposing (Triangle3d)
import UbiquitousTypes exposing (LocalCoords)
import Vector3d


type alias ControlPoint =
    Point3d Length.Meters LocalCoords


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
            -- This is used for wrap-around on loop, in which case use second point
            if loopiness == IsALoop then
                List.take 1 <| List.drop 1 points

            else
                List.take 1 points

        lastPoint =
            -- This is used for wrap-around on loop, in which case use penultimate point
            if loopiness == IsALoop then
                List.take 1 <| List.drop 1 <| List.reverse points

            else
                List.take 1 <| List.reverse points

        numPoints =
            List.length points

        loopedPoints =
            points ++ firstPoint

        filteredLoop =
            List.map3
                (weightedAverage filterBias)
                (lastPoint ++ points)
                points
                (List.drop 1 points ++ firstPoint)

        filtered =
            List.map3
                (weightedAverage filterBias)
                (firstPoint ++ points)
                points
                (List.drop 1 points ++ lastPoint)

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
    let
        triangle =
            Triangle3d.fromVertices ( p0.xyz, p1.xyz, p2.xyz )

        centroid =
            Triangle3d.centroid triangle

        newP1 =
            Point3d.interpolateFrom p1.xyz centroid bias

        ( lon, lat, ele ) =
            toGPXcoords newP1
    in
    { p1
        | lon = lon
        , lat = lat
        , ele = ele
        , xyz = newP1
    }


bezierSplines : Bool -> Float -> Float -> List ControlPoint -> List ControlPoint
bezierSplines isLoop tension tolerance points =
    let
        firstPoint =
            -- This is used for wrap-around on loop, in which case use second point
            if isLoop then
                List.take 1 <| List.drop 1 points

            else
                List.take 1 points

        lastPoint =
            -- This is used for wrap-around on loop, in which case use penultimate point
            if isLoop then
                List.take 1 <| List.drop 1 <| List.reverse points

            else
                List.take 1 <| List.reverse points

        makeTriangles : List (Triangle3d Length.Meters LocalCoords)
        makeTriangles =
            let
                shiftedBack =
                    if isLoop then
                        lastPoint ++ points

                    else
                        firstPoint ++ points

                shiftedForwards =
                    if isLoop then
                        List.drop 1 points ++ firstPoint

                    else
                        List.drop 1 points ++ lastPoint
            in
            List.map3
                Triangle3d.from
                shiftedBack
                points
                shiftedForwards

        controlPointsFromTriangle :
            Triangle3d Length.Meters LocalCoords
            -> ( ControlPoint, ControlPoint, ControlPoint )
        controlPointsFromTriangle triangle =
            let
                ( _, b, _ ) =
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
            -> CubicSpline3d Length.Meters LocalCoords
        makeSpline ( _, start, control1 ) ( control2, end, _ ) =
            CubicSpline3d.fromControlPoints
                start
                control1
                control2
                end

        makeSplines : List (CubicSpline3d Length.Meters LocalCoords)
        makeSplines =
            List.map2
                makeSpline
                makeControlPoints
                (List.drop 1 makeControlPoints)

        asPolylines : List (Polyline3d Length.Meters LocalCoords)
        asPolylines =
            List.map
                (CubicSpline3d.approximate (Length.meters tolerance))
                makeSplines

        asSegments : List (LineSegment3d Length.Meters LocalCoords)
        asSegments =
            List.concatMap
                Polyline3d.segments
                asPolylines

        asPointsAgain : List ControlPoint
        asPointsAgain =
            List.map
                LineSegment3d.startPoint
                (List.take 1 asSegments)
                ++ List.map
                    LineSegment3d.endPoint
                    asSegments
    in
    asPointsAgain
