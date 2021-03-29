module StravaPasteStreams exposing (..)

import NodesAndRoads exposing (deriveNodes, deriveTrackPointBox)
import StravaTypes exposing (StravaSegment, StravaSegmentStreams)
import TrackPoint exposing (TrackPoint, reindexTrackpoints, singleton, trackPointFromLatLon)


pasteStreams : List TrackPoint -> StravaSegment -> StravaSegmentStreams -> List TrackPoint
pasteStreams trackPoints segment streams =
    let
        pStartingTrackPoint =
            -- Our first track point will be replaced with the first stream point
            trackPointFromLatLon segment.start_latitude segment.start_longitude trackPoints

        pEndingTrackPoint =
            -- Our last track point will be replaced with the last stream point
            trackPointFromLatLon segment.end_latitude segment.end_longitude trackPoints


        newRoute =
            case ( pStartingTrackPoint, pEndingTrackPoint ) of
                ( Just startingTrackPoint, Just endingTrackPoint ) ->
                    let
                        start =
                            startingTrackPoint.idx

                        finish =
                            endingTrackPoint.idx

                        trackPointsFromStreams =
                            List.map2
                                (\latLon ele ->
                                    { singleton
                                        | lat = latLon.lat
                                        , lon = latLon.lng
                                        , ele = ele
                                        , xyz = TrackPoint.fromGPXcoords latLon.lng latLon.lat ele
                                        , idx = start
                                    }
                                )
                                streams.latLngs.data
                                streams.altitude.data

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
    newRoute
