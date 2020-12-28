module TrackPoint exposing (..)

import BoundingBox3d
import Element exposing (..)
import Json.Encode as E
import Msg exposing (..)
import Point3d
import Regex
import Utils exposing (asRegex)


type GPXCoords
    = GPXCoords

type alias TrackPoint =
    -- This is the basic info we extract from a GPX file.
    { lat : Float
    , lon : Float
    , ele : Float
    , idx : Int
    }

singleton = { lat = 0.0, lon = 0.0, ele = 0.0, idx = 0 }

singletonPoint = Point3d.origin

singletonBox = BoundingBox3d.singleton singletonPoint

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


viewTrackPoint : TrackPoint -> Element Msg
viewTrackPoint trkpnt =
    column [ padding 5, spacing 5 ]
        [ text <| "Lat:" ++ String.fromFloat trkpnt.lat
        , text <| "Lon:" ++ String.fromFloat trkpnt.lon
        , text <| "Ele:" ++ String.fromFloat trkpnt.ele
        ]


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


trackToJSON : List TrackPoint -> E.Value
trackToJSON tps =
    -- JSON suitable for Mapbox API.
    let
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            List.map latLonPair tps

        latLonPair tp =
            E.list E.float [ tp.lon, tp.lat ]
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


