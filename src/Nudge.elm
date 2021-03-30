module Nudge exposing (..)

import Angle
import Length
import Point2d
import Spherical exposing (metresPerDegree)
import TrackPoint exposing (TrackPoint, fromGPXcoords)
import Vector2d


nudgeTrackPoint : TrackPoint -> Float -> Float -> TrackPoint
nudgeTrackPoint baseTP horizontal vertical =
    let
        roadVector =
            -- The negation because, no idea.
            Vector2d.rTheta (Length.meters 1.0)
                (Angle.radians <| -1.0 * baseTP.naturalBearing)
                |> Vector2d.rotateClockwise

        nudgeVector =
            Vector2d.perpendicularTo roadVector
                |> Vector2d.scaleBy (horizontal / metresPerDegree)

        trackPoint2d =
            Point2d.meters baseTP.lon baseTP.lat

        nudgedTrackPoint2d =
            Point2d.translateBy nudgeVector trackPoint2d

        ( lon, lat ) =
            Point2d.toTuple Length.inMeters nudgedTrackPoint2d
    in
    { baseTP
        | lat = lat
        , lon = lon
        , ele = baseTP.ele + vertical
        , xyz = fromGPXcoords lon lat (baseTP.ele + vertical)
    }
