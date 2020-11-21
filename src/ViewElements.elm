module ViewElements exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Msg exposing (Msg(..))


prettyButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 16
    , Border.color <| rgb255 0x50 0x50 0x50
    , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = rgb255 0xD0 0xD0 0xD0 }
    , Background.color <| rgb255 114 159 207
    , Font.color <| rgb255 0xFF 0xFF 0xFF
    , mouseOver
        [ Background.color <| rgb255 0xFF 0xFF 0xFF, Font.color <| rgb255 0 0 0 ]
    , focused
        [ Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = rgb255 114 159 207 } ]
    ]


loadButton =
    button
        prettyButtonStyles
        { onPress = Just GpxRequested
        , label = text "Load GPX from your computer"
        }
