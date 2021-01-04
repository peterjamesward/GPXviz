module StravaDataLoad exposing (..)

import Http
import Json.Decode as D exposing (Decoder)
import OAuth exposing (Token, useToken)
import StravaTypes exposing (..)


requestStravaSegment : (Result Http.Error StravaSegment -> msg) -> String -> Token -> Cmd msg
requestStravaSegment msg segmentId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = "https://www.strava.com/api/v3/segments/" ++ segmentId
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


requestStravaRoute : (Result Http.Error String -> msg) -> String -> Token -> Cmd msg
requestStravaRoute msg routeId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = "https://www.strava.com/api/v3/routes/" ++ routeId ++ "/export_gpx"
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
