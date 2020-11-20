module BendSmoother exposing (..)

import Arc2d
import Geometry101 as G exposing (..)
import Spherical exposing (findBearingToTarget)
import TrackPoint exposing (TrackPoint)


type alias SmoothedBend =
    { trackPoints : List TrackPoint
    , centre : ( Float, Float )
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


bendIncircle : Float -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> Maybe SmoothedBend
bendIncircle maxTurn pa pb pc pd =
    -- Given the actual road between two markers, this will
    -- return a suggested smoothed bend, if it exists.
    -- First argument here is the maximum angle between generated road segments.
    -- Hence smaller values results in a closer approximation to the incircle.
    -- ** Note that the road direction is important in selecting the circle arc. **
    -- We can (and shall) use Arc2d.sweptAround, and the same module to interpolate for us!
    let
        maybeCircle : Maybe G.Circle
        maybeCircle =
            G.findIncircleFromTwoRoads (roadToGeometry pa pb) (roadToGeometry pc pd)
    in
    case maybeCircle of
        Just circle ->
            let
                entryPoint =
                    G.findTangentPoint (roadToGeometry pa pb) circle

                exitPoint =
                    G.findTangentPoint (roadToGeometry pc pd) circle

            in
            case ( entryPoint, exitPoint ) of
                ( Just p1, Just p2 ) ->
                    let
                        t1 =
                            toTrackPoint p1

                        t2 =
                            toTrackPoint p2
                    in
                    Just
                        { trackPoints =
                            [ pa
                            , { t1 | ele = pa.ele }
                            , { t2 | ele = pc.ele }
                            , pd
                            ]
                        , centre = ( circle.centre.x, circle.centre.y )
                        , radius = circle.radius
                        }

                _ ->
                    Nothing

        _ ->
            Nothing
