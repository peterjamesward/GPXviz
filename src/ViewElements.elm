module ViewElements exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Msg exposing (Msg(..))
import Utils exposing (showDecimal2)


withMouseCapture =
    [ htmlAttribute <| Pointer.onDown (\event -> ImageGrab event.pointer.offsetPos)
    , htmlAttribute <| Pointer.onMove (\event -> ImageRotate event.pointer.offsetPos)
    , htmlAttribute <| Pointer.onUp (\event -> ImageRelease event.pointer.offsetPos)
    , htmlAttribute <| style "touch-action" "none"
    , width fill
    , pointer
    ]


displayName n =
    case n of
        Just s ->
            el [ Font.size 24, padding 8 ]
                (text s)

        _ ->
            none


distanceFromZoom : Float -> Float
distanceFromZoom zoomLevel =
    10.0 ^ (5.0 - zoomLevel)


type ButtonPosition
    = First
    | Mid
    | Last


radioButton position label state =
    let
        borders =
            case position of
                First ->
                    { left = 2, right = 2, top = 2, bottom = 2 }

                Mid ->
                    { left = 0, right = 2, top = 2, bottom = 2 }

                Last ->
                    { left = 0, right = 2, top = 2, bottom = 2 }

        corners =
            case position of
                First ->
                    { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                Mid ->
                    { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

                Last ->
                    { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }
    in
    el
        [ padding 10
        , spacing 2
        , Border.widthEach { left = 2, right = 2, top = 2, bottom = 0 }
        , Border.roundEach { topLeft = 10, bottomLeft = 0, topRight = 10, bottomRight = 0 }
        , Border.color <|
            if state == Input.Selected then
                rgb255 150 200 50

            else
                rgb255 255 255 255
        , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = rgb255 0xD0 0xD0 0xD0 }
        , Background.color <|
            if state == Input.Selected then
                rgb255 50 150 50

            else
                rgb255 114 159 207
        , Font.color <| rgb255 0xFF 0xFF 0xFF
        ]
    <|
        el [ centerX, centerY ] <|
            text label


zoomSlider value msg =
    Input.slider
        [ height <| px 400
        , width <| px 80
        , alignTop
        , behindContent <|
            -- Slider track
            el
                [ width <| px 30
                , height <| px 400
                , alignTop
                , centerX
                , Background.color <| rgb255 114 159 207
                , Border.rounded 6
                ]
                Element.none
        ]
        { onChange = msg
        , label =
            Input.labelHidden "Zoom"
        , min = 1.0
        , max = 4.0
        , step = Nothing
        , value = value
        , thumb = Input.defaultThumb
        }


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


checkboxIcon : Bool -> Element msg
checkboxIcon isChecked =
    el
        [ width <| px 32
        , height <| px 32
        , centerY
        , padding 4
        , Border.rounded 6
        , Border.width 2
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    rgb255 114 159 207

                else
                    rgb255 0xFF 0xFF 0xFF
            ]
        <|
            none


commonShortHorizontalSliderStyles =
    [ height <| px 30
    , width <| px 200
    , centerY
    , behindContent <|
        -- Slider track
        el
            [ width <| px 200
            , height <| px 30
            , centerY
            , centerX
            , Background.color <| rgb255 114 159 207
            , Border.rounded 6
            ]
            Element.none
    ]


commonShortVerticalSliderStyles =
    [ height <| px 200
    , width <| px 30
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 30
            , height <| px 200
            , centerY
            , centerX
            , Background.color <| rgb255 114 159 207
            , Border.rounded 6
            ]
            Element.none
    ]


straightenButton c m =
    button
        prettyButtonStyles
        { onPress = Just (StraightenStraight c m)
        , label =
            text <|
                "Straighten between markers"
        }


nudgeButton c horizontalValue verticalValue =
    button
        prettyButtonStyles
        { onPress = Just (NudgeNode c horizontalValue verticalValue)
        , label =
            text <|
                "Apply nudge"
        }


splitButton c =
    button
        prettyButtonStyles
        { onPress = Just (SplitRoad c)
        , label =
            text <|
                "Divide this segment\ninto two"
        }


horizontalNudgeSlider c value =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetHorizontalNudgeFactor c
        , label =
            Input.labelBelow [] <|
                text <|
                    "Offset = "
                        ++ showDecimal2 value
                        ++ "m"
        , min = -5.0
        , max = 5.0
        , step = Nothing
        , value = value
        , thumb = Input.defaultThumb
        }


verticalNudgeSlider c value =
    el [ width <| px 100, centerX ] <|
        Input.slider
            commonShortVerticalSliderStyles
            { onChange = SetVerticalNudgeFactor c
            , label =
                Input.labelBelow [ centerX ] <|
                    text <|
                        "Height = "
                            ++ showDecimal2 value
                            ++ "m"
            , min = -5.0
            , max = 5.0
            , step = Nothing
            , value = value
            , thumb = Input.defaultThumb
            }
