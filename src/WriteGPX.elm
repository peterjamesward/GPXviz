module WriteGPX exposing (writeGPX)

preamble = """
<?xml version='1.0' encoding='UTF-8'?>
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

writeTrackPoint tp =
      <trkpt lat="51.615646" lon="-0.301302">
        <ele>83.626787</ele>
      </trkpt>

writeTrack name trackPoints=
    """
  <trk>
    <name>""" ++ name ++ """</name>
    <trkseg>"""
    ++ List.concatMap writeTrackPoint trackPoints ++
  """    </trkseg>
  </trk>
 """

writeFooter =
</gpx>

writeGPX =
    writePreamble
    ++ writeTrack
    ++ writeFooter