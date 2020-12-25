module ViewTypes exposing (..)


type ViewingMode
    = FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView
    | ProfileView
    | PlanView
    | MapView



type alias Coordinate =
    { lon : Float
    , lat : Float
    }
