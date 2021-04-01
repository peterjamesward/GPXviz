module TrackPoint exposing (..)

import Angle exposing (Angle)
import Area
import BoundingBox3d
import Dict
import Geometry101 exposing (interpolateScalar)
import Json.Encode as E
import Length
import Point3d exposing (Point3d)
import Regex
import Set
import Spherical exposing (metresPerDegree)
import Triangle3d
import UbiquitousTypes exposing (LocalCoords)
import Utils exposing (asRegex)


type GPXCoords
    = GPXCoords


type alias TrackPoint =
    -- This is the basic info we extract from a GPX file. Lat and Lon in degrees.
    -- I'm now adding more stuff, with a view to removing need for DrawingNode.
    { lat : Float
    , lon : Float
    , ele : Float
    , idx : Int
    , naturalBearing : Float -- average of bearings either side.
    , xyz : Point3d.Point3d Length.Meters LocalCoords
    , costMetric : Float -- impact if this node is removed, by some measure.
    }


singleton =
    { lat = 0.0
    , lon = 0.0
    , ele = 0.0
    , idx = 0
    , naturalBearing = 0
    , costMetric = 0.0
    , xyz = Point3d.origin
    }


singletonPoint =
    Point3d.origin


singletonBox =
    BoundingBox3d.singleton singletonPoint


interpolateSegment : Float -> TrackPoint -> TrackPoint -> TrackPoint
interpolateSegment w startTP endTP =
    -- We work in TrackPoints so that everything has a lat, lon and ele.
    -- Everything else derives from those three coordinates.
    let
        startPoint =
            Point3d.fromTuple Length.meters
                ( startTP.lon, startTP.lat, startTP.ele )

        endPoint =
            Point3d.fromTuple Length.meters
                ( endTP.lon, endTP.lat, endTP.ele )

        newPoint =
            Point3d.interpolateFrom startPoint endPoint w

        interpolatedLocalCoords =
            Point3d.interpolateFrom startTP.xyz endTP.xyz w

        ( x, y, z ) =
            Point3d.toTuple Length.inMeters newPoint
    in
    { lat = y
    , lon = x
    , ele = z
    , idx = startTP.idx
    , naturalBearing = interpolateScalar 0.5 startTP.naturalBearing endTP.naturalBearing
    , xyz = interpolatedLocalCoords
    , costMetric = 0
    }


reindexTrackpoints : List TrackPoint -> List TrackPoint
reindexTrackpoints trackPoints =
    -- Extra info is for indexing into the To-Be graph structure.
    -- Also going to work out the cost metric and the "natural bearing" here.
    let
        helper reversed nextIdx points =
            -- Note the interesting point here is the second one. The first is context.
            case points of
                [ penultimate, last ] ->
                    -- We can wrap things up now
                    helper
                        ({ last
                            | idx = nextIdx + 1
                            , costMetric = 10 ^ 10
                            , naturalBearing = trackPointBearing penultimate last
                         }
                            :: reversed
                        )
                        (nextIdx + 1)
                        [ { last
                            | idx = nextIdx
                            , costMetric = 10 ^ 10
                            , naturalBearing = trackPointBearing penultimate last
                          }
                        ]

                previous :: point :: next :: rest ->
                    helper
                        ({ point
                            | idx = nextIdx
                            , naturalBearing =
                                -- Yes, this means we do all the sums twice :(
                                meanBearing
                                    (trackPointBearing previous point)
                                    (trackPointBearing point next)
                            , costMetric =
                                Area.inSquareMeters <|
                                    Triangle3d.area <|
                                        Triangle3d.fromVertices
                                            ( previous.xyz, point.xyz, next.xyz )
                         }
                            :: reversed
                        )
                        (nextIdx + 1)
                        (point :: next :: rest)

                _ ->
                    -- No more work, just flip the accumulation list.
                    List.reverse reversed
    in
    case trackPoints of
        firstPoint :: secondPoint :: morePoints ->
            helper
                [ { firstPoint
                    | idx = 0
                    , costMetric = 10 ^ 10 -- i.e. do not remove me!
                    , naturalBearing = trackPointBearing firstPoint secondPoint
                  }
                ]
                1
                trackPoints

        _ ->
            []


meanBearing : Float -> Float -> Float
meanBearing b1 b2 =
    -- Average bearings but cater for the awkward pi/-pi flip.
    if (b1 < -pi / 2 && b2 > pi / 2) || (b2 < -pi / 2 && b1 > pi / 2) then
        (b1 + b2) / 2.0 - pi

    else
        (b1 + b2) / 2.0


fromGPXcoords : Float -> Float -> Float -> Point3d Length.Meters LocalCoords
fromGPXcoords lon lat ele =
    Point3d.fromTuple
        Length.meters
        ( metresPerDegree * lon * cos (degrees lat)
        , metresPerDegree * lat
        , ele
        )


toGPXcoords : Point3d Length.Meters LocalCoords -> ( Float, Float, Float )
toGPXcoords point =
    let
        ( x, y, ele ) =
            Point3d.toTuple Length.inMeters point

        lat =
            y / metresPerDegree

        lon =
            x / metresPerDegree / cos (degrees lat)
    in
    ( lon, lat, ele )


trackPointAsPoint tp =
    Point3d.meters tp.lon tp.lat tp.ele


trackPointBoundingBox points =
    -- This is a good reason for the bbox to be a Maybe B~
    case points of
        tp1 :: tps ->
            BoundingBox3d.hull
                (trackPointAsPoint tp1)
                (List.map trackPointAsPoint tps)

        _ ->
            BoundingBox3d.singleton Point3d.origin


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
                        , naturalBearing = 0
                        , xyz = fromGPXcoords lon lat ele -- Pending bounding box adjustment
                        , costMetric = 0 -- fill in later
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

        parsedPoints =
            List.filterMap identity <| List.map trackPoint trkpts

        box =
            --TODO: This will replace the Node calculation from where it's copied.
            case parsedPoints of
                tp1 :: tps ->
                    BoundingBox3d.hull
                        tp1.xyz
                        (List.map .xyz tps)

                _ ->
                    BoundingBox3d.singleton Point3d.origin

        ( midLon, midLat, _ ) =
            --(0,0,0)
            Point3d.toTuple Length.inMeters <|
                BoundingBox3d.centerPoint box

        correctXYZ tp =
            let
                ( x, y, z ) =
                    Point3d.toTuple Length.inMeters tp.xyz
            in
            { tp | xyz = Point3d.fromTuple Length.meters ( x - midLon, y - midLat, z ) }
    in
    --List.map correctXYZ
    parsedPoints


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
    -- Filter on proximity then sort to get nearest.
    let
        withinTolerance tp =
            abs (tp.lon - lon)
                < (2.0 / metresPerDegree)
                && abs (tp.lat - lat)
                < (2.0 / metresPerDegree)
    in
    tps
        |> List.filter withinTolerance
        |> trackPointFromLatLon lat lon


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
