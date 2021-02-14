module ViewPureStyles exposing (..)

import ColourPalette exposing (buttonBackground, buttonShadow, buttonText)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


prettyButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 16
    , Border.color buttonBackground

    --, Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = rgb255 0xD0 0xD0 0xD0 }
    , Background.color buttonBackground
    , Font.color <| buttonText
    , Font.size 16
    , mouseOver
        [ Background.color buttonText, Font.color buttonBackground ]
    , focused
        [ Border.shadow { offset = ( 4, 0 ), size = 3, blur = 5, color = buttonShadow } ]
    , centerX
    ]
