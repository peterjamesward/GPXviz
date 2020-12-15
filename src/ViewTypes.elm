module ViewTypes exposing (..)


type ViewingMode
    = FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView
    | ProfileView
    | PlanView
    | MapView


type alias MapInfo =
    { center : Coordinate
    , zoom : Float
    }

type alias Coordinate =
    { lon : Float
    , lat : Float
    }
