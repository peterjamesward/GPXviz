module BendSmoother exposing (..)

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
                        ( firstTangentAngle, secondTangentAngle ) =
                            ( circleAngle circle p1, circleAngle circle p2 )

                        ( v1, v2 ) =
                            ( asVector p1 circle.centre, asVector p2 circle.centre )

                        totalAngle =
                            --secondTangentAngle - firstTangentAngle
                            angle v2 v1

                        pointIndices =
                            -- We have already the tangent points
                            -- so here we need the intermediates
                            -- e.g. for four points in all, here we want [1,2]
                            List.range 1 (numPoints - 2)

                        pointsIn01 =
                            -- [0.33, 0.66]. We're dividing by three because four points means three increments.
                            pointIndices
                                |> List.map
                                    (\i -> toFloat i / (toFloat numPoints - 1.0))

                        angleInterpolate x =
                            firstTangentAngle + totalAngle * x

                        ourPointAngles =
                            List.map angleInterpolate pointsIn01

                        ourPoints =
                            List.map (pointOnCircle circle) ourPointAngles

                        trackPoints =
                            List.map toTrackPoint ourPoints
                    in
                    Just
                        { trackPoints = [ pa, toTrackPoint p1 ] ++ trackPoints ++ [ toTrackPoint p2, pd ]
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
