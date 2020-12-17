port module MapController exposing (..)


type MapState
    = WaitingForNode
    | WaitingForMapLoad
    | MapLoaded
    | MapStopping
    | MapStopped

