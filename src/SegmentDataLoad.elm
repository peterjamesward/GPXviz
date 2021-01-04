module SegmentDataLoad exposing (..)

import Http
import Json.Decode as D exposing (Decoder)
import OAuth exposing (Token, useToken)
import StravaSegment exposing (StravaSegment)


requestStravaSegment : (Result Http.Error StravaSegment -> msg) -> String -> Token -> Cmd msg
requestStravaSegment msg segmentId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = "https://www.strava.com/api/v3/segments/" ++ segmentId
        , body = Http.emptyBody
        , expect = Http.expectJson msg stravaSegmentDecorder
        , timeout = Nothing
        , tracker = Nothing
        }


stravaSegmentDecorder : D.Decoder StravaSegment
stravaSegmentDecorder =
    D.map8 StravaSegment
        (D.at [ "name" ] D.string)
        (D.at [ "distance" ] D.float)
        (D.at [ "elevation_high" ] D.float)
        (D.at [ "elevation_low" ] D.float)
        (D.at [ "start_latitude" ] D.float)
        (D.at [ "start_longitude" ] D.float)
        (D.at [ "end_latitude" ] D.float)
        (D.at [ "end_longitude" ] D.float)
