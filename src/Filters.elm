module Filters exposing (applyWeightedAverageFilter)

import Array
import List.Extra
import Loop exposing (..)
import TrackPoint exposing (TrackPoint)



applyWeightedAverageFilter : Int -> Int -> Loopiness -> List TrackPoint -> List TrackPoint
applyWeightedAverageFilter start finish loopiness points =
    let
        firstPoint =
            List.take 1 points

        numPoints =
            List.length points

        loopedPoints =
            List.Extra.cycle (numPoints * 2) points

        filteredLoop =
            List.map5
                (weightedAverage start finish)
                (List.drop (numPoints - 2) loopedPoints)
                (List.drop (numPoints - 1) loopedPoints)
                points
                (List.drop 1 loopedPoints)
                (List.drop 2 loopedPoints)

        filtered =
            List.map5
                (weightedAverage start finish)
                (firstPoint ++ firstPoint ++ points) -- So first point is averaged with itself.
                (firstPoint ++ points)
                points
                (List.drop 1 loopedPoints)
                (List.drop 2 loopedPoints)

        withinRange =
                Array.fromList
                >> Array.slice (start + 1) finish
                >> Array.toList

        ( fixedFirst, fixedLast ) =
            ( List.take (start + 1) points, List.drop finish points )
    in
    case loopiness of
        IsALoop ->
            --TODO: First and last points must remain together!
            --TODO: That they don't, suggests a logic bug.
            fixedFirst ++ (withinRange filteredLoop) ++ fixedLast

        _ ->
            fixedFirst ++ (withinRange filtered) ++ fixedLast


weightedAverage :
    Int
    -> Int
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
weightedAverage start finish p0 p1 p2 p3 p4 =
    if p2.idx >= start && p2.idx <= finish then
        { lon = withWeights .lon p0 p1 p2 p3 p4
        , lat = withWeights .lat p0 p1 p2 p3 p4
        , ele = withWeights .ele p0 p1 p2 p3 p4
        , idx = p2.idx
        }

    else
        p2


withWeights :
    (TrackPoint -> Float)
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> Float
withWeights f p0 p1 p2 p3 p4 =
    let
        ( x0, x1, x2 ) =
            ( f p0, f p1, f p2 )

        ( x3, x4 ) =
            ( f p3, f p4 )
    in
    (x0 + x1 * 2 + x2 * 4 + x3 * 2 + x4) / 10.0
