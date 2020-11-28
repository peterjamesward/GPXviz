module DisplayOptions exposing (..)


type CurtainStyle
    = NoCurtain
    | PlainCurtain
    | RainbowCurtain
    | PastelCurtain


type alias DisplayOptions =
    { roadPillars : Bool
    , roadCones : Bool
    , roadTrack : Bool
    , curtainStyle : CurtainStyle
    , problems : Bool
    , centreLine : Bool
    , terrain : Bool
    }


defaultDisplayOptions : DisplayOptions
defaultDisplayOptions =
    { roadPillars = True
    , roadCones = True
    , roadTrack = True
    , curtainStyle = PastelCurtain
    , problems = False
    , centreLine = False
    , terrain = False
    }
