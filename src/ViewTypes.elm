module ViewTypes exposing (..)


type ViewingMode
    = OverviewView
    | FirstPersonView
    | ThirdPersonView
    | AboutView
    | InputErrorView
    | ProfileView
    | PlanView


type ViewSubmode
    = ShowData
    | ShowGradientFixes
    | ShowBendFixes
    | ShowNodeTools

