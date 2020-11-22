module ViewTypes exposing (..)


type ViewingMode
    = OverviewView
    | FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView
    | ProfileView


type ThirdPersonSubmode
    = ShowData
    | ShowGradientFixes
    | ShowBendFixes

