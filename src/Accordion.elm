module Accordion exposing (..)

-- Seeking a better way to organise all the controls.

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)


type AccordionState
    = Expanded
    | Contracted
    | Disabled


type AccordionMessage
    = AccordionMessage


type alias AccordionEntry msg =
    { label : String
    , state : AccordionState
    , content : Element msg
    }


accordionMenuStyles =
    [ padding 10
    , alignTop
    , width fill
    ]


accordionRowStyles =
    [ padding 10
    , Border.widthEach { left = 2, right = 2, top = 2, bottom = 0 }
    , Border.roundEach { topLeft = 6, bottomLeft = 0, topRight = 6, bottomRight = 0 }
    , Border.color <| rgb255 100 100 100
    , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = rgb255 0xD0 0xD0 0xD0 }
    , Background.color <| rgb255 50 150 50
    , Font.color <| rgb255 0xFF 0xFF 0xFF
    ]


accordionToggle :
    List (AccordionEntry msg)
    -> AccordionEntry msg
    -> List (AccordionEntry msg)
accordionToggle entries entry =
    let
        _ =
            Debug.log "Toggle " entry

        toggleMatching e =
            if e.label == entry.label then
                { e
                    | state =
                        if e.state == Expanded then
                            Contracted

                        else
                            Expanded
                }

            else
                { e | state = Contracted }
    in
    List.map toggleMatching entries


accordionView :
    List (AccordionEntry msg)
    -> (AccordionEntry msg -> msg)
    -> Element msg
accordionView entries message =
    let
        entryButton : AccordionEntry msg -> Element msg
        entryButton entry =
            button accordionRowStyles
                { onPress = Just (message entry)
                , label = text entry.label
                }
    in
    column accordionMenuStyles
        [ wrappedRow accordionMenuStyles (List.map entryButton entries)
        , case accordionActiveItem entries of
            Just entry ->
                entry.content

            Nothing ->
                none
        ]


accordionActiveItem : List (AccordionEntry msg) -> Maybe (AccordionEntry msg)
accordionActiveItem entries =
    List.head <| List.filter (\e -> e.state == Expanded) entries
