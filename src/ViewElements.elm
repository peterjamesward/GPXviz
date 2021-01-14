module ViewElements exposing (..)

import ColourPalette exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Attributes exposing (style)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Msg exposing (Msg(..))
import OAuthTypes exposing (OAuthMsg(..))
import Utils exposing (scrollbarThickness, showDecimal2)


withMouseCapture =
    [ htmlAttribute <| Mouse.onDown (\event -> ImageGrab event)
    , htmlAttribute <| Mouse.onMove (\event -> ImageDrag event)
    , htmlAttribute <| Mouse.onUp (\event -> ImageRelease event)
    , htmlAttribute <| Mouse.onClick (\event -> MouseClick event)
    , htmlAttribute <| Mouse.onDoubleClick (\event -> MouseDoubleClick event)
    , htmlAttribute <| Wheel.onWheel (\event -> MouseWheel event.deltaY)
    , htmlAttribute <| style "touch-action" "none"
    , onContextMenu NoOpMsg
    , width fill
    , pointer
    ]


onContextMenu : Msg -> Element.Attribute Msg
onContextMenu msg =
    HE.custom "contextmenu"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


displayName n =
    case n of
        Just s ->
            el [ Font.size 20 ]
                (text s)

        _ ->
            none


type ButtonPosition
    = First
    | Mid
    | Last


radioButton position label state =
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
        , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = radioButtonShadow }
        , Background.color <|
            if state == Input.Selected then
                radioButtonSelected

            else
                radioButtonDefault
        , Font.color radioButtonText
        , Font.size 16
        ]
    <|
        el [ centerX, centerY ] <|
            text label


zoomSlider value msg =
    Input.slider
        [ height <| px 400
        , width <| px scrollbarThickness
        , alignTop
        , behindContent <|
            -- Slider track
            el
                [ width <| px scrollbarThickness
                , height <| px 400
                , alignTop
                , centerX
                , Background.color scrollbarBackground
                , Border.rounded 6
                ]
                Element.none
        ]
        { onChange = msg
        , label =
            Input.labelHidden "Zoom"
        , min = 0.0
        , max = 22.0
        , step = Nothing
        , value = value
        , thumb = Input.defaultThumb
        }


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
        , Border.color buttonShadow
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    buttonBackground

                else
                    collapsedTabBorder
            ]
        <|
            none


commonShortHorizontalSliderStyles =
    [ height <| px 20
    , width <| px 150
    , centerY
    , behindContent <|
        -- Slider track
        el
            [ width <| px 150
            , height <| px 20
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


commonShortVerticalSliderStyles =
    [ height <| px 150
    , width <| px 20
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 20
            , height <| px 150
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


straightenButton : Element Msg
straightenButton =
    button
        prettyButtonStyles
        { onPress = Just StraightenStraight
        , label =
            text <|
                "Straighten between markers"
        }


nudgeButton : Float -> Float -> Element Msg
nudgeButton horizontalValue verticalValue =
    button
        prettyButtonStyles
        { onPress = Just (NudgeNode horizontalValue verticalValue)
        , label =
            text <|
                "Apply nudge"
        }


splitSegmentOptions maxSegLength =
    let
        splitSlider value =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = SetMaxTrackpointSpacing
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Maximum gap = "
                                ++ showDecimal2 value
                                ++ "m"
                , min = 5.0
                , max = 50.0
                , step = Just 5.0
                , value = value
                , thumb = Input.defaultThumb
                }
    in
    row [ spacing 10 ]
        [ splitSlider maxSegLength
        , button
            prettyButtonStyles
            { onPress = Just SplitRoad
            , label =
                text <|
                    "Add track points"
            }
        ]


deleteNodeButton ( start, finish ) =
    let
        message =
            if start == finish then
                "Delete current point"

            else
                "Delete from " ++ String.fromInt start ++ " to " ++ String.fromInt finish
    in
    button
        prettyButtonStyles
        { onPress = Just (DeleteTrackPoints (start, finish))
        , label = text message
        }


horizontalNudgeSlider value =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetHorizontalNudgeFactor
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


verticalNudgeSlider value =
    el [ width <| px 80, centerX ] <|
        Input.slider
            commonShortVerticalSliderStyles
            { onChange = SetVerticalNudgeFactor
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


compatibleWithStrava =
    image
        [ height (px 50), width (px 100), alignLeft ]
        { src = "images/api_logo_cptblWith_strava_stack_gray.svg"
        , description = "Compatible with Strava"
        }
