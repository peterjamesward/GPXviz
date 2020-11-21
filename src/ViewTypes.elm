module ViewTypes exposing (..)


type ViewingMode
    = OverviewView
    | FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView


type ThirdPersonSubmode
    = ShowData
    | ShowGradientFixes
    | ShowBendFixes

