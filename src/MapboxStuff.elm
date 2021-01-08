module MapboxStuff exposing (metresPerPixel, zoomLevelFromBoundingBox)

import BoundingBox3d exposing (BoundingBox3d)
import Length
import Point3d
import Spherical exposing (metresPerDegree)
import TrackPoint exposing (GPXCoords)
import Utils exposing (viewMapHeight, viewMapWidth)


metresPerPixelAtEquatorZoomZero =
    78271.484


metresPerPixel zoomLevel latitude =
    cos latitude * metresPerPixelAtEquatorZoomZero / 2.0 ^ zoomLevel


zoomLevelFromBoundingBox : BoundingBox3d Length.Meters GPXCoords -> Float
zoomLevelFromBoundingBox box =
    let
        centre =
            BoundingBox3d.centerPoint box

        latitude =
            degrees <| Length.inMeters <| Point3d.yCoordinate centre

        ( longitudes, latitudes, _ ) =
            BoundingBox3d.dimensions box

        horizontalMetresPerPixel =
            metresPerDegree * Length.inMeters longitudes / viewMapWidth

        verticalMetresPerPixel =
            metresPerDegree * Length.inMeters latitudes / viewMapHeight

        desiredMetresPerPixel =
            1.5 * (max horizontalMetresPerPixel verticalMetresPerPixel)

        zoom =
            logBase 2 (cos latitude * metresPerPixelAtEquatorZoomZero / desiredMetresPerPixel)
    in
    clamp 0.0 22.0 <|
        zoom
