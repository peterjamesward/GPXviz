module TrackPoint exposing (..)

import BoundingBox3d
import Dict
import Json.Encode as E
import Length
import Point3d exposing (Point3d)
import Regex
import Set
import Spherical exposing (metresPerDegree)
import Utils exposing (asRegex)


type GPXCoords
    = GPXCoords


type alias TrackPoint =
    -- This is the basic info we extract from a GPX file. Angles in degrees.
    { lat : Float
    , lon : Float
    , ele : Float
    , idx : Int
    }


type alias GraphNode =
    -- Beginning of trying to interpret the road as a simple Euclidean node-edge network.
    -- Experimentally, a GraphNode is a TrackPoint with three or more distinct neighbours.
    -- (and the start and finish).
    { location : ( Float, Float )
    , neighbours : List ( Float, Float )
    }


singleton =
    { lat = 0.0, lon = 0.0, ele = 0.0, idx = 0 }


singletonPoint =
    Point3d.origin


singletonBox =
    BoundingBox3d.singleton singletonPoint


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


parseTrackPoints : String -> List TrackPoint
parseTrackPoints xml =
    let
        trkpts =
            Regex.find (asRegex "<trkpt((.|\\n|\\r)*?)trkpt>") xml |> List.map .match

        latitude trkpt =
            Regex.find (asRegex "lat=\\\"([\\d\\.-]*)\\\"") trkpt |> matches

        longitude trkpt =
            Regex.find (asRegex "lon=\\\"([\\d\\.-]*)\\\"") trkpt |> matches

        elevation trkpt =
            Regex.find (asRegex "<ele>([\\d\\.-]*)<\\/ele>") trkpt |> matches

        trackPoint : String -> Maybe TrackPoint
        trackPoint trkpt =
            case ( latitude trkpt, longitude trkpt, elevation trkpt ) of
                ( (Just lat) :: _, (Just lon) :: _, (Just ele) :: _ ) ->
                    Just
                        { lat = lat
                        , lon = lon
                        , ele = ele
                        , idx = 0
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
    List.filterMap identity <|
        List.map
            trackPoint
            trkpts


trackToJSON : List TrackPoint -> E.Value
trackToJSON tps =
    -- JSON suitable for Mapbox API to add polyline for route.
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


trackPointsToJSON : List TrackPoint -> E.Value
trackPointsToJSON tps =
    -- Similar but each point is a feature so it is draggable.
    --var geojson = {
    --    'type': 'FeatureCollection',
    --    'features': [
    --        {
    --            'type': 'Feature',
    --            'geometry': {
    --                'type': 'Point',
    --                'coordinates': [0, 0]
    --            }
    --        }
    --    ]
    --};
    let
        features =
            List.map makeFeature tps

        makeFeature tp =
            E.object
                [ ( "type", E.string "Feature" )
                , ( "geometry", point tp )
                ]

        point tp =
            E.object
                [ ( "type", E.string "Point" )
                , ( "coordinates", latLonPair tp )
                ]

        latLonPair tp =
            E.list E.float [ tp.lon, tp.lat ]
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]


removeByNodeNumbers : List Int -> List TrackPoint -> List TrackPoint
removeByNodeNumbers idxsToRemove trackPoints =
    -- Both input lists are sorted in index order.
    let
        ( _, _, retained ) =
            helper idxsToRemove trackPoints []

        helper idxs tps kept =
            case ( idxs, tps ) of
                ( [], _ ) ->
                    ( [], [], List.reverse tps ++ kept )

                ( _, [] ) ->
                    ( [], [], kept )

                ( i :: is, t :: ts ) ->
                    if t.idx == i then
                        helper is ts kept

                    else if t.idx < i then
                        helper idxs ts (t :: kept)

                    else
                        -- t.idx > i
                        helper is tps kept
    in
    List.reverse retained


findTrackPoint : Float -> Float -> List TrackPoint -> Maybe TrackPoint
findTrackPoint lon lat tps =
    -- Originally for when a map point is dragged, so the lon & lat are accurate.
    -- (It's not a nearest search but may need second pass if tolerance is too big.)
    let
        withinTolerance tp =
            abs (tp.lon - lon)
                < (2.0 / metresPerDegree)
                && abs (tp.lat - lat)
                < (2.0 / metresPerDegree)
    in
    List.head <|
        List.filter withinTolerance tps


meanTrackPoint tp0 tp1 =
    { lat = (tp0.lat + tp1.lat) / 2.0
    , lon = (tp0.lon + tp1.lon) / 2.0
    , ele = (tp0.ele + tp1.ele) / 2.0
    , idx = tp0.idx
    }


filterCloseTrackPoints : List TrackPoint -> List TrackPoint
filterCloseTrackPoints tps =
    let
        helper : TrackPoint -> List TrackPoint -> List TrackPoint -> List TrackPoint
        helper tPrev stack tTail =
            case tTail of
                [] ->
                    List.reverse stack

                t1 :: ts ->
                    if trackPointSeparation tPrev t1 < 0.1 then
                        helper tPrev stack ts

                    else
                        helper t1 (t1 :: stack) ts
    in
    case tps of
        [] ->
            []

        t0 :: tRest ->
            helper t0 [ t0 ] tRest


trackPointSeparation tp1 tp2 =
    Spherical.range ( tp1.lat, tp1.lon ) ( tp2.lat, tp2.lon )


trackPointFromLatLon : Float -> Float -> List TrackPoint -> Maybe TrackPoint
trackPointFromLatLon lat lon tps =
    -- Why do I have the feeling I've already written this?
    let
        dummyTP =
            { lat = lat, lon = lon }
    in
    tps
        |> List.map (\tp -> ( tp, trackPointSeparation tp dummyTP ))
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.head


trackPointBearing : TrackPoint -> TrackPoint -> Float
trackPointBearing tp1 tp2 =
    Spherical.findBearingToTarget
        ( tp1.lat, tp1.lon )
        ( tp2.lat, tp2.lon )


pointFromTrackpoint : TrackPoint -> Point3d Length.Meters GPXCoords
pointFromTrackpoint tp =
    Point3d.fromMeters { x = tp.lon, y = tp.lat, z = tp.ele }


pointAsTrackPoint : Point3d Length.Meters GPXCoords -> TrackPoint
pointAsTrackPoint p =
    let
        { x, y, z } =
            Point3d.toMeters p
    in
    { lat = y, lon = x, ele = z, idx = 0 }

