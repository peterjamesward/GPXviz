module Filters exposing (applyWeightedAverageFilter)

import Array
import List.Extra
import Loop exposing (..)
import TrackPoint exposing (TrackPoint)


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
    -> ( Bool, Bool )
    -> Loopiness
    -> List TrackPoint
    -> List TrackPoint
applyWeightedAverageFilter ( start, finish ) ( filterXY, filterZ ) loopiness points =
    let
        firstPoint =
            List.take 1 points

        numPoints =
            List.length points

        loopedPoints =
            points ++ List.drop 1 points

        ( latFn, lonFn, eleFn ) =
            ( if filterXY then
                withWeights

              else
                centreValue
            , if filterXY then
                withWeights

              else
                centreValue
            , if filterZ then
                withWeights

              else
                centreValue
            )

        filteredLoop =
            List.map5
                (weightedAverage ( latFn, lonFn, eleFn ))
                (List.drop (numPoints - 2) loopedPoints)
                (List.drop (numPoints - 1) loopedPoints)
                points
                (List.drop 1 loopedPoints)
                (List.drop 2 loopedPoints)

        filtered =
            List.map5
                (weightedAverage ( latFn, lonFn, eleFn ))
                (firstPoint ++ firstPoint ++ points)
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
    if start == finish && loopiness == IsALoop then
        filteredLoop

    else
        fixedFirst ++ withinRange filtered ++ fixedLast


weightedAverage :
    ( FilterFunction, FilterFunction, FilterFunction )
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
weightedAverage ( latFn, lonFn, eleFn ) p0 p1 p2 p3 p4 =
    { lon = lonFn .lon p0 p1 p2 p3 p4
    , lat = latFn .lat p0 p1 p2 p3 p4
    , ele = eleFn .ele p0 p1 p2 p3 p4
    , idx = p2.idx
    }


withWeights : FilterFunction
withWeights f p0 p1 p2 p3 p4 =
    let
        ( x0, x1, x2 ) =
            ( f p0, f p1, f p2 )

        ( x3, x4 ) =
            ( f p3, f p4 )
    in
    (x0 + x1 * 2 + x2 * 4 + x3 * 2 + x4) / 10.0


centreValue : FilterFunction
centreValue f _ _ p2 _ _ =
    f p2
