module AutoFix exposing (..)

import TrackPoint exposing (TrackPoint, interpolateSegment, meanTrackPoint, reindexTrackpoints, trackPointSeparation)


autoFix : List TrackPoint -> List Int -> List TrackPoint
autoFix tps nodesToFix =
    reindexTrackpoints <|
        autoFixInternal tps nodesToFix


autoFixInternal :
    List TrackPoint -- original track point list
    -> List Int -- list of TPs to be fixed (somehow)
    -> List TrackPoint -- The outcome!
autoFixInternal inputs nodesToFix =
    -- Both input lists are sorted in index order.
    case ( inputs, nodesToFix ) of
        ( tp0 :: tp1 :: tp2 :: tpRest, n1 :: nRest ) ->
            if tp1.idx == n1 then
                chamfer tp0 tp1 tp2
                    ++ autoFixInternal
                        tpRest
                        nRest

            else if tp1.idx < n1 then
                tp0 :: autoFixInternal (tp1 :: tp2 :: tpRest) nodesToFix

            else
                -- somehow nodes out of order
                autoFixInternal inputs nRest

        ( _, _ ) ->
            inputs


chamfer : TrackPoint -> TrackPoint -> TrackPoint -> List TrackPoint
chamfer tp0 tp1 tp2 =
    -- Goal is to replace tp1 with two trackpoints, such that we smooth bend slightly.
    --TODO:
    -- Exactly what the two-for-one trackpoint button does. So this code should become
    -- the reference code one it works.
    -- If the triangle area is less than 2 sqm, just delete t1. But we're in GPXCoords here.
    let
        t0t1Length =
            trackPointSeparation tp0 tp1

        t1t2Length =
            trackPointSeparation tp1 tp2

        t0t2Length =
            trackPointSeparation tp0 tp2

        amountToStealFromFirstSegment =
            t0t1Length / 2.0

        amountToStealFromSecondSegment =
            t1t2Length / 2.0

        commonAmountToSteal =
            min amountToStealFromFirstSegment amountToStealFromSecondSegment

        firstTP =
            interpolateSegment
                (commonAmountToSteal / t0t1Length)
                tp1
                tp0

        secondTP =
            interpolateSegment
                (commonAmountToSteal / t1t2Length)
                tp1
                tp2

        cosineT1 =
            --Cosine rule c2 = a2 + b2 - 2ab cos C
            (t0t1Length ^ 2 + t1t2Length ^ 2 - t0t2Length ^ 2) / (2.0 * t0t1Length * t1t2Length)
    in
    if t0t2Length < 2.0 then
        -- no point adding points in a small gap
        [ tp0, meanTrackPoint tp0 tp2, tp2 ]

    else if cosineT1 > cos (pi / 2) then
        -- there should be no acute angles.
        [ tp0, meanTrackPoint tp0 tp2, tp2 ]

    else
        [ tp0, firstTP, secondTP, tp2 ]
