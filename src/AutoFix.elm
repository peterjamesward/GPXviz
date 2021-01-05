module AutoFix exposing (..)

import TrackPoint exposing (TrackPoint, interpolateSegment, reindexTrackpoints, trackPointSeparation)


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
                (chamfer tp0 tp1 tp2) ++
                    (autoFixInternal
                        (tp2 :: tpRest )
                        nRest )

            else if tp1.idx < n1 then
                tp0 :: tp1 :: autoFixInternal (tp1 :: tpRest) nodesToFix

            else -- somehow nodes out of order
                autoFixInternal inputs nRest

        ( _, _ ) ->
            inputs


chamfer : TrackPoint -> TrackPoint -> TrackPoint -> List TrackPoint
chamfer tp0 tp1 tp2 =
    -- Goal is to replace tp1 with two trackpoints, such that we smooth bend slightly.
    --TODO:
    -- Exactly what the two-for-one trackpoint button does. So this code should become
    -- the reference code one it works.
    let
        t0t1Length = trackPointSeparation tp0 tp1

        t1t2Length = trackPointSeparation tp1 tp2

        amountToStealFromFirstSegment =
            (t0t1Length / 2.0)

        amountToStealFromSecondSegment =
            (t1t2Length / 2.0)

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
    in
    [ tp0, firstTP, secondTP, tp2 ]