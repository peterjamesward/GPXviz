module StravaPasteStreams exposing (..)

import StravaTypes exposing (StravaSegment, StravaSegmentStreams)
import TrackPoint exposing (TrackPoint, reindexTrackpoints, trackPointFromLatLon)


pasteStreams : List TrackPoint -> StravaSegment -> StravaSegmentStreams -> List TrackPoint
pasteStreams trackPoints segment streams =
    let
        pStartingTrackPoint =
            -- Our first track point will be replaced with the first stream point
            trackPointFromLatLon segment.start_latitude segment.start_longitude trackPoints

        pEndingTrackPoint =
            -- Our last track point will be replaced with the last stream point
            trackPointFromLatLon segment.end_latitude segment.end_longitude trackPoints

        trackPointsFromStreams =
            List.map2
                (\latLon ele -> TrackPoint latLon.lat latLon.lng ele 0)
                streams.latLngs.data
                streams.altitude.data
    in
    case ( pStartingTrackPoint, pEndingTrackPoint ) of
        ( Just startingTrackPoint, Just endingTrackPoint ) ->
            let
                precedingPoints =
                    List.take startingTrackPoint.idx trackPoints

                remainingPoints =
                    List.drop (endingTrackPoint.idx + 1) trackPoints
            in
            reindexTrackpoints <|
                precedingPoints
                    ++ trackPointsFromStreams
                    ++ remainingPoints

        _ ->
            trackPoints
