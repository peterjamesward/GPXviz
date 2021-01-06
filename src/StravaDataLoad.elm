module StravaDataLoad exposing (..)

import Http
import Json.Decode as D exposing (Decoder, field)
import OAuth exposing (Token, useToken)
import StravaTypes exposing (..)
import Url.Builder as Builder


stravaApiRoot =
    "https://www.strava.com"


requestStravaSegment : (Result Http.Error StravaSegment -> msg) -> String -> Token -> Cmd msg
requestStravaSegment msg segmentId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "segments", segmentId ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg stravaSegmentDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


stravaSegmentDecoder : D.Decoder StravaSegment
stravaSegmentDecoder =
    D.map8 StravaSegment
        (D.at [ "name" ] D.string)
        (D.at [ "distance" ] D.float)
        (D.at [ "elevation_high" ] D.float)
        (D.at [ "elevation_low" ] D.float)
        (D.at [ "start_latitude" ] D.float)
        (D.at [ "start_longitude" ] D.float)
        (D.at [ "end_latitude" ] D.float)
        (D.at [ "end_longitude" ] D.float)


requestStravaSegmentStreams : (Result Http.Error StravaSegmentStreams -> msg) -> String -> Token -> Cmd msg
requestStravaSegmentStreams msg segmentId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "segments", segmentId, "streams" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg decodeStravaSegmentStreams
        , timeout = Nothing
        , tracker = Nothing
        }


requestStravaRoute : (Result Http.Error String -> msg) -> String -> Token -> Cmd msg
requestStravaRoute msg routeId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "routes", routeId, "export_gpc" ] []
        , body = Http.emptyBody
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }


stravaRouteDecoder : D.Decoder StravaRoute
stravaRouteDecoder =
    D.map4 StravaRoute
        (D.at [ "name" ] D.string)
        (D.at [ "description" ] D.string)
        (D.at [ "distance" ] D.float)
        (D.at [ "elevation_gain" ] D.float)


decodeStravaSegmentStreams : D.Decoder StravaSegmentStreams
decodeStravaSegmentStreams =
    D.map3 StravaSegmentStreams
        (field "0" decodeLatLngStream)
        (field "1" decodeStravaDistanceStream)
        (field "2" decodeStravaAltitudeStream)


decodeStravaLatLng : D.Decoder StravaLatLng
decodeStravaLatLng =
    D.map2 StravaLatLng
        (field "0" D.float)
        (field "1" D.float)


decodeLatLngStream : D.Decoder StravaLatLngStream
decodeLatLngStream =
    D.map5 StravaLatLngStream
        (field "type" D.string)
        (field "data" (D.list decodeStravaLatLng))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)


decodeStravaDistanceStream : D.Decoder StravaDistanceStream
decodeStravaDistanceStream =
    D.map5 StravaDistanceStream
        (field "type" D.string)
        (field "data" (D.list D.float))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)


decodeStravaAltitudeStream : D.Decoder StravaAltitudeStream
decodeStravaAltitudeStream =
    D.map5 StravaAltitudeStream
        (field "type" D.string)
        (field "data" (D.list D.float))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)
