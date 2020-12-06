module ScalingInfo exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Length

type GPXCoord = GPXCoord

type alias ScalingInfo =
    { box : BoundingBox3d Length.Meters GPXCoord -- This is bounding box of TrackPoints, not Nodes!!
    }
