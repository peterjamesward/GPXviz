module BendSmoother exposing (..)

import Geometry101 as G exposing (..)
import TrackPoint exposing (TrackPoint)


type alias SmoothedBend =
    { trackPoints : List TrackPoint
    , centre : (Float, Float)
    , radius : Float
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


bendIncircle : List TrackPoint -> Maybe SmoothedBend
bendIncircle tps =
    -- Given the actual road between two markers, this will
    -- return a suggested smoothed bend, if it exists.
    -- The input convention is that the first two trackpoints designate the lead-in road segment,
    -- and the last two trackpoints designate the exit segment.
    -- There must be at least three trackpoints to form a bend.
    case ( tps, List.reverse tps ) of
        ( [ pa, pb, _ ], [ pz, py, _ ] ) ->
            let
                maybeCircle : Maybe G.Circle
                maybeCircle =
                    G.findIncircleFromTwoRoads (roadToGeometry pa pb) (roadToGeometry py pz)
            in
            case maybeCircle of
                Just circle ->
                    let
                        entryPoint =
                            G.findTangentPoint (roadToGeometry pa pb) circle

                        exitPoint =
                            G.findTangentPoint (roadToGeometry py pz) circle
                    in
                    case ( entryPoint, exitPoint ) of
                        ( Just p1, Just p2 ) ->
                            Just
                                { trackPoints =
                                    [ pa
                                    , toTrackPoint p1
                                    , toTrackPoint p2
                                    , pz
                                    ]
                                , centre = (circle.centre.x, circle.centre.y)
                                , radius = circle.radius
                                }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
