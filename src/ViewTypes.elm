module ViewTypes exposing (..)


type ViewingMode
    = FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView
    | ProfileView
    | PlanView
    | MapView
    | ConnectionsView


type alias Coordinate =
    { lon : Float
    , lat : Float
    }
