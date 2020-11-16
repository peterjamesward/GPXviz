module WriteGPX exposing (writeGPX)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import TrackPoint exposing (..)


preamble =
    """<?xml version='1.0' encoding='UTF-8'?>
<gpx version="1.1"
  creator="https://www.komoot.de"
  xmlns="http://www.topografix.com/GPX/1/1"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.topografix.com/GPX/1/1
  http://www.topografix.com/GPX/1/1/gpx.xsd">
  <metadata>
    <name>Cycling</name>
    <author>
      <link href="https://www.stepwiserefinement.co.uk">
        <text>Fixed by Pete and his GPX viewer</text>
        <type>text/html</type>
      </link>
    </author>
  </metadata>
"""


writePreamble =
    preamble


writeTrackPoint : TrackPoint -> String
writeTrackPoint tp =
    "<trkpt lat=\""
        ++ decimals6 tp.lat
        ++ "\" lon=\""
        ++ decimals6 tp.lon
        ++ "\">"
        ++ "<ele>"
        ++ decimals6 tp.ele
        ++ "</ele>"
        ++ "</trkpt>\n"


writeTrack : String -> List TrackPoint -> String
writeTrack name trackPoints =
    """
  <trk>
    <name>"""
        ++ name
        ++ """</name>
    <trkseg>
"""
        ++ String.concat (List.map writeTrackPoint trackPoints)
        ++ """    </trkseg>
  </trk>
 """


writeFooter =
    "</gpx>"


writeGPX : Maybe String -> List TrackPoint -> String
writeGPX name track =
    let
        useName =
            case name of
                Just n ->
                    n

                _ ->
                    "A track"
    in
    writePreamble
        ++ writeTrack useName track
        ++ writeFooter


decimals6 x =
    let
        locale =
            { usLocale
                | decimals = Exact 6
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x
