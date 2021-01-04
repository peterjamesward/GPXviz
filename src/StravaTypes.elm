module StravaTypes exposing (..)


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


type alias StravaRoute =
    {- "distance" : 0.8008282,
       "description" : "aeiou",
       "elevation_gain" : 6.0274563,
       "name" : "aeiou",
    -}
    { name : String
    , description : String
    , distance : Float
    , elevation_gain : Float
    }
