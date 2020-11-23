module BendSmoother exposing (..)

import Angle exposing (normalize)
import Geometry101 as G exposing (..)
import TrackPoint exposing (TrackPoint)


type alias SmoothedBend =
    { trackPoints : List TrackPoint
    , centre : ( Float, Float )
    , radius : Float
    , turnAngle : Float
    , firstTangentAngle : Float
    , secondTangentAngle : Float
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


bendIncircle : Int -> TrackPoint -> TrackPoint -> TrackPoint -> TrackPoint -> Maybe SmoothedBend
bendIncircle numPoints pa pb pc pd =
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

        fromOrigin circ pt =
            -- Express point as vector from circle centre, for finding angles.
            { x = pt.x - circ.centre.x, y = pt.y - circ.centre.y }

        det v1 v2 =
            v1.x * v2.x + v1.y * v2.y

        dot v1 v2 =
            v1.x * v2.y - v1.y * v2.x

        angle v1 v2 =
            atan2 (det v1 v2) (dot v1 v2)

        circleAngle circ p =
            atan2 (p.y - circ.centre.y) (p.x - circ.centre.x)

        pointOnCircle circ a =
            { x = circ.centre.x + circ.radius * cos a
            , y = circ.centre.y + circ.radius * sin a
            }
    in
    case maybeCircle of
        Just circle ->
            let
                ( entryPoint, exitPoint ) =
                    ( G.findTangentPoint (roadToGeometry pa pb) circle
                    , G.findTangentPoint (roadToGeometry pc pd) circle
                    )
            in
            case ( entryPoint, exitPoint ) of
                ( Just p1, Just p2 ) ->
                    let
                        ( t1, t2 ) =
                            ( toTrackPoint p1, toTrackPoint p2 )

                        ( firstTangentAngle, secondTangentAngle ) =
                            ( circleAngle circle p1, circleAngle circle p2 )

                        totalAngle =
                            secondTangentAngle - firstTangentAngle

                        extraPoints =
                            -- In addition to the known tangent points.
                            numPoints - 2

                        turnPerPoint =
                            -- 5 points, 4 gaps.
                            totalAngle / toFloat (numPoints - 1)

                        newPoints =
                            List.map (toTrackPoint << newPoint) <| List.range 1 extraPoints

                        newPoint i =
                            pointOnCircle circle <|
                                firstTangentAngle
                                    - turnPerPoint
                                    * toFloat i
                    in
                    Just
                        { trackPoints =
                            [ pa, t1 ]
                                ++ newPoints
                                ++ [ t2, pd ]
                        , centre = ( circle.centre.x, circle.centre.y )
                        , radius = circle.radius
                        , turnAngle = totalAngle
                        , firstTangentAngle = firstTangentAngle
                        , secondTangentAngle = secondTangentAngle
                        }

                _ ->
                    Nothing

        _ ->
            Nothing
