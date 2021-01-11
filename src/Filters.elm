module Filters exposing (applyWeightedAverageFilter)

import TrackPoint exposing (TrackPoint)


applyWeightedAverageFilter : List TrackPoint -> List TrackPoint
applyWeightedAverageFilter points =
    List.map5
        weightedAverage
        (List.take 2 points ++ points)
        (List.take 1 points ++ points)
        points
        (List.drop 1 points)
        (List.drop 2 points)
        ++ (List.reverse <| List.take 2 <| List.reverse points)


weightedAverage : TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint
weightedAverage p0 p1 p2 p3 p4 =
    { lon = withWeights .lon p0 p1 p2 p3 p4
    , lat = withWeights .lat p0 p1 p2 p3 p4
    , ele = withWeights .ele p0 p1 p2 p3 p4
    , idx = p2.idx
    }


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
