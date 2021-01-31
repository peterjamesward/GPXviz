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

