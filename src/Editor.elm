module Editor exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

-- I'm going to migrate current functionality to this before creating the editing suite.

import TrackPoint exposing (TrackPoint)


type Direction
    = Forwards
    | Backwards


type alias Clip =
    { trackPoints : List TrackPoint
    , sectionName : String -- so they can be saved to file?
    }


type alias Appearance =
    { clip : Clip
    , direction : Direction
    , centrelineOffset : Float -- metres right (-ve == left) of centreline.
    }


type alias Scene =
    -- Not sure if Direction & Offset should be here [also].
    { appearances : List Appearance
    , repeats : Maybe Int -- Just n <=> it makes a loop.
    }


type alias Film =
    { scenes : List Scene }
