module ViewTypes exposing (..)


type ViewingMode
    = OverviewView
    | FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView
    | ProfileView
    | PlanView


type ThirdPersonSubmode
    = ShowData
    | ShowGradientFixes
    | ShowBendFixes

