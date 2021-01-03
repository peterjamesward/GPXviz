module SegmentDataLoad exposing (..)

import Http
import Json.Decode as D exposing (Decoder)
import StravaSegment exposing (StravaSegment)

--TODO: Move this into the StravaAuth module.
requestStravaSegment : (Result Http.Error StravaSegment -> msg) -> String -> Cmd msg
requestStravaSegment msg segmentId =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" "Bearer c0a09444251c8098a777cad2d7ef961ad02de84d"
            ]
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
