module Filters exposing (applyGaussianSmoothing)

import Array
import List.Extra
import Loop exposing (..)
import TrackPoint exposing (TrackPoint)
import Zipper exposing (Zipper(..))


type alias FilterFunction =
    Float
    -> (TrackPoint -> Float)
    -> TrackPoint
    -> List TrackPoint
    -> Float


applyGaussianSmoothing :
    ( Int, Int )
    -> ( Float, Float )
    -> Bool
    -> Loopiness
    -> List TrackPoint
    -> List TrackPoint
applyGaussianSmoothing ( start, finish ) ( sigmaXYZ, sigmaZ ) zOnly loopiness points =
    -- Using my old filter as a framework but Gaussian is more involved:
    -- 1. Add track points so that spacing is less than sigma.
    -- 2. Smooth XYZ or Z only
    -- 3. Remove co-linear points (using existing cost metric)
    -- 4. Stitch into anchored points if necessary (because smoothing inevitable draws in the end points).
    -- Don't worry about efficiency. Deal with that if we need to.
    let
        zipper =
            case points of
                pFirst :: pRest ->
                    Just (Zipper [] pFirst pRest)

                _ ->
                    Nothing

        ( latFn, lonFn, eleFn ) =
            if zOnly then
                ( centreValue, centreValue, withWeights sigmaZ )

            else
                ( withWeights sigmaXYZ, withWeights sigmaXYZ, withWeights sigmaXYZ )

        filtered zipper =
            Zipper.toList <|
                Zipper.extend
                    (gaussianKernel ( latFn, lonFn, eleFn ) sigmas)
                    zipper

        withinRange =
            Array.fromList
                >> Array.slice (start + 1) finish
                >> Array.toList

        ( fixedFirst, fixedLast ) =
            ( List.take (start + 1) points, List.drop finish points )
    in
    points
        |> isolateAffectedPoints
        |> interpolatePoints (min sigmaXYZ sigmaZ)
        |> smoothPoints
        |> removeRedundantPoints
        |> spliceIntoRoute
        |> reindex


gaussianKernel :
    ( FilterFunction, FilterFunction, FilterFunction )
    -> Zipper TrackPoint
    -> Maybe TrackPoint
gaussianKernel ( latFn, lonFn, eleFn ) zp =
    -- We shall gather track points with 2.sigma each side and apply convolution.
    -- So passing in a zipper would be quite neat here.
    { lon = lonFn .lon p0 ps
    , lat = latFn .lat p0 ps
    , ele = eleFn .ele p0 ps
    , idx = p0.idx
    }


withWeights : FilterFunction
withWeights sigma f x xs =
    let
        ( x0, x1, x2 ) =
            ( f p0, f p1, f p2 )

        ( x3, x4 ) =
            ( f p3, f p4 )
    in
    (x0 + x1 * 2 + x2 * 4 + x3 * 2 + x4) / 10.0


centreValue : FilterFunction
centreValue f x0 _ =
    f x0
