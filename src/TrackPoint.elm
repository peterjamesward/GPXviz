module TrackPoint exposing (..)

type alias TrackPoint =
    -- This is the basic info we extract from a GPX file.
    { lat : Float
    , lon : Float
    , ele : Float
    , idx : Int
    }

dummyTrackPoint =
        { lat = 0.0
        , lon = 0.0
        , ele = 0.0
        , idx = 0
        }
