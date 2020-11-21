module DisplayOptions exposing (..)


type CurtainStyle
    = NoCurtain
    | PlainCurtain
    | RainbowCurtain


type alias DisplayOptions =
    { roadPillars : Bool
    , roadCones : Bool
    , roadTrack : Bool
    , curtainStyle : CurtainStyle
    , problems : Bool
    }


defaultDisplayOptions : DisplayOptions
defaultDisplayOptions =
    { roadPillars = True
    , roadCones = True
    , roadTrack = True
    , curtainStyle = RainbowCurtain
    , problems = False
    }
