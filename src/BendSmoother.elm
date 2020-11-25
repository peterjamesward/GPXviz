module BendSmoother exposing (..)

import Arc2d exposing (throughPoints)
import Geometry101 as G exposing (..)
import Length
import LineSegment2d
import Point2d exposing (xCoordinate, yCoordinate)
import Polyline2d
import Spherical exposing (metresPerDegreeLatitude)
import TrackPoint exposing (TrackPoint)



--TODO: Interpolate elevations.


type alias SmoothedBend =
    { trackPoints : List TrackPoint
    , centre : ( Float, Float )
    , radius : Float
    , startIndex : Int -- First trackpoint to be replaced
    , endIndex : Int -- and last
    }


roadToGeometry : TrackPoint -> TrackPoint -> G.Road
roadToGeometry startsAt endsAt =
    { startAt = { x = startsAt.lon, y = startsAt.lat }
    , endsAt = { x = endsAt.lon, y = endsAt.lat }
    }


toTrackPoint : Point -> TrackPoint
toTrackPoint p =
    -- NOTE caller has to fix up the missing fields!
    { lat = p.y
    , lon = p.x
    , ele = 0.0
    , idx = 0
    }


toPoint tp =
    { x = tp.lon, y = tp.lat }


bendIncircle : Int -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> Maybe SmoothedBend
bendIncircle numSegments pa pb pc pd =
    -- Given the actual road between two markers, this will
    -- return a suggested smoothed bend, if it exists.
    -- First argument here is the maximum angle between generated road segments.
    -- Hence smaller values results in a closer approximation to the incircle.
    -- ** Note that the road direction is important in selecting the circle arc. **
    -- We can (and shall) use Arc2d, and the same module to interpolate for us!
    let
        ( r1, r2 ) =
            ( roadToGeometry pa pb, roadToGeometry pc pd )

        maybeP =
            findIntercept r1 r2

        maybeCircle : Maybe G.Circle
        maybeCircle =
            G.findIncircleFromTwoRoads r1 r2
    in
    case ( maybeCircle, maybeP ) of
        ( Just circle, Just p ) ->
            let
                ( entryPoint, exitPoint ) =
                    ( G.findTangentPoint r1 circle
                    , G.findTangentPoint r2 circle
                    )
            in
            case ( entryPoint, exitPoint ) of
                ( Just p1, Just p2 ) ->
                    -- Solved the tangent points.
                    -- We'll use Arc2d to find our approximation.
                    -- First, we find an intermediate point on the circle,
                    -- intersecting with the line connecting the centre (known) to
                    -- the intersection of the tangents (p).
                    let
                        distCentreToP =
                            distance circle.centre p

                        proportion =
                            circle.radius / distCentreToP

                        midArcPoint =
                            interpolateLine proportion circle.centre p

                        maybeArc =
                            Arc2d.throughPoints
                                (Point2d.meters p1.x p1.y)
                                (Point2d.meters midArcPoint.x midArcPoint.y)
                                (Point2d.meters p2.x p2.y)

                        t1 =
                            -- Elevations of tangent points are interpolations
                            -- wthin the segments that contain them.
                            { lat = p1.y
                            , lon = p1.x
                            , ele =
                                interpolateScalar
                                    (whatFraction p1 (toPoint pa) (toPoint pb))
                                    pa.ele
                                    pb.ele
                            }

                        t2 =
                            -- Elevations of tangent points are interpolations
                            -- wthin the segments that contain them.
                            { lat = p2.y
                            , lon = p2.x
                            , ele =
                                interpolateScalar
                                    (whatFraction p2 (toPoint pc) (toPoint pd))
                                    pc.ele
                                    pd.ele
                            , idx = 0
                            }

                        eleIncrement =
                            (t2.ele - t1.ele) / toFloat numSegments
                    in
                    case maybeArc of
                        Just arc ->
                            -- Found the arc we're looking for.
                            -- Now find approximation with line segments.
                            let
                                segments =
                                    Arc2d.segments numSegments arc
                                        |> Polyline2d.segments

                                newTrackPoints =
                                    List.map2
                                        (\seg i ->
                                            let
                                                p0 =
                                                    LineSegment2d.startPoint seg
                                            in
                                            { lon = xCoordinate p0 |> Length.inMeters
                                            , lat = yCoordinate p0 |> Length.inMeters
                                            , ele = t1.ele + toFloat i * eleIncrement
                                            , idx = 0
                                            }
                                        )
                                        segments
                                        (List.range 0 numSegments)
                            in
                            Just
                                { trackPoints =
                                    pa
                                        :: newTrackPoints
                                        ++ [ t2, pd ]
                                , centre = ( circle.centre.x, circle.centre.y )
                                , radius = circle.radius * metresPerDegreeLatitude
                                , startIndex = pa.idx
                                , endIndex = pd.idx
                                }

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
