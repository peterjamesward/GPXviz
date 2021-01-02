module StravaSegment exposing (..)


type alias StravaSegment =
    { name : String
    , distance : Float
    , elevation_high : Float
    , elevation_low : Float
    , start_latitude : Float
    , start_longitude : Float
    , end_latitude : Float
    , end_longitude : Float
    }
