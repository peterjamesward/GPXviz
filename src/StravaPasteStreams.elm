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

        newRoute =
            case ( pStartingTrackPoint, pEndingTrackPoint ) of
                ( Just startingTrackPoint, Just endingTrackPoint ) ->
                    let
                        start =
                            startingTrackPoint.idx

                        finish =
                            endingTrackPoint.idx

                        orientedSegment =
                            if start == finish then
                                []

                            else if start < finish then
                                trackPointsFromStreams

                            else
                                List.reverse trackPointsFromStreams

                        precedingPoints =
                            List.take (min start finish) trackPoints

                        remainingPoints =
                            List.drop (max start finish + 1) trackPoints
                    in
                    precedingPoints
                        ++ orientedSegment
                        ++ remainingPoints

                _ ->
                    trackPoints
    in
    reindexTrackpoints newRoute
