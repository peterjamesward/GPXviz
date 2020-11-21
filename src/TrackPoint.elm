module TrackPoint exposing (..)


import Regex
import Spherical exposing (metresPerDegreeLatitude)
import Utils exposing (asRegex)
type alias TrackPoint =
    -- This is the basic info we extract from a GPX file.
    { lat : Float
    , lon : Float
    , ele : Float
    , idx : Int
    }


type alias ScalingInfo =
    { mins : TrackPoint
    , maxs : TrackPoint
    , centres : TrackPoint
    , largestDimension : Float -- biggest bounding box edge determines scaling factor
    , seaLevelInClipSpace : Float
    , metresToClipSpace : Float -- Probably should be a proper metric tag!
    }


dummyTrackPoint =
    { lat = 0.0
    , lon = 0.0
    , ele = 0.0
    , idx = 0
    }


interpolateSegment : Float -> TrackPoint -> TrackPoint -> TrackPoint
interpolateSegment w startTP endTP =
    -- We work in TrackPoints so that everything has a lat, lon and ele.
    -- Everything else derives from those three coordinates.
    let
        ( x1, y1, z1 ) =
            ( startTP.lon, startTP.lat, startTP.ele )

        ( x2, y2, z2 ) =
            ( endTP.lon, endTP.lat, endTP.ele )

        ( x, y, z ) =
            ( w * x2 + (1.0 - w) * x1
            , w * y2 + (1.0 - w) * y1
            , w * z2 + (1.0 - w) * z1
            )
    in
    { lat = y
    , lon = x
    , ele = z
    , idx = 0
    }


reindexTrackpoints points =
    List.map2
        (\p i -> { p | idx = i })
        points
        (List.range 0 (List.length points))


deriveScalingInfo : List TrackPoint -> ScalingInfo
deriveScalingInfo tps =
    let
        lowerBounds tp =
            { lat = Maybe.withDefault 0.0 <| List.minimum <| List.map .lat tp
            , lon = Maybe.withDefault 0.0 <| List.minimum <| List.map .lon tp
            , ele = Maybe.withDefault 0.0 <| List.minimum <| List.map .ele tp
            , idx = 0
            }

        upperBounds tp =
            { lat = Maybe.withDefault 0.0 <| List.maximum <| List.map .lat tp
            , lon = Maybe.withDefault 0.0 <| List.maximum <| List.map .lon tp
            , ele = Maybe.withDefault 0.0 <| List.maximum <| List.map .ele tp
            , idx = 0
            }

        mins =
            lowerBounds tps

        maxs =
            upperBounds tps

        findCentres =
            { lat = (mins.lat + maxs.lat) / 2.0
            , lon = (mins.lon + maxs.lon) / 2.0
            , ele = (mins.ele + maxs.ele) / 2.0
            , idx = 0
            }

        scalingFactor =
            max (maxs.lat - mins.lat) (maxs.lon - mins.lon)

        metresToClipSpace =
            1 / (0.5 * scalingFactor * metresPerDegreeLatitude)

        elevationToClipSpace e =
            (e - findCentres.ele) * metresToClipSpace
    in
    { mins = mins
    , maxs = maxs
    , centres = findCentres
    , largestDimension = scalingFactor
    , metresToClipSpace = metresToClipSpace
    , seaLevelInClipSpace = elevationToClipSpace 0.0
    }


parseTrackPoints : String -> List TrackPoint
parseTrackPoints xml =
    let
        latitudes =
            Regex.find (asRegex "lat=\\\"([\\d\\.-]*)\\\"") xml |> matches

        longitudes =
            Regex.find (asRegex "lon=\\\"([\\d\\.-]*)\\\"") xml |> matches

        elevations =
            Regex.find (asRegex "<ele>([\\d\\.-]*)<\\/ele>") xml |> matches

        makeTrackPoint mayLat mayLon mayEle idx =
            case ( mayLat, mayLon, mayEle ) of
                ( Just a, Just b, Just c ) ->
                    Just
                        { lat = a
                        , lon = b
                        , ele = c
                        , idx = idx
                        }

                _ ->
                    Nothing

        matches xs =
            List.map value xs

        value x =
            case x.submatches of
                (Just val) :: _ ->
                    String.toFloat val

                _ ->
                    Nothing
    in
    List.map4
        makeTrackPoint
        latitudes
        longitudes
        elevations
        (List.range 0 (List.length latitudes))
        |> List.filterMap identity

