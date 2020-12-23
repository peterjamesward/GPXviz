module Utils exposing (..)

import Color
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Pixels
import Regex


type alias Point =
    ( Float, Float )


toDegrees rads =
    rads * 180.0 / pi


bearingToDisplayDegrees x =
    showDecimal2 <|
        toDegrees <|
            if x < 0 then
                pi + pi + x

            else
                x


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


gradientColourVivid slope =
    -- Note we want (say) 15% to be maximum Red, flat is Green, -15% purple.
    let
        x =
            (clamp -15.0 15.0 slope + 15.0) / 30.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 1.0 0.4


gradientColourPastel slope =
    let
        x =
            (clamp -15.0 15.0 slope + 15.0) / 30.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 0.6 0.7


asRegex t =
    -- Helper to make a regex pattern.
    Maybe.withDefault Regex.never <| Regex.fromString t


parseTrackName xml =
    case Regex.find (asRegex "<name>(.*)<\\/name>") xml of
        [] ->
            Nothing

        x :: _ ->
            case x.submatches of
                [] ->
                    Nothing

                n :: _ ->
                    n


view3dDimensions =
    ( Pixels.int 800, Pixels.int 600 )

view3dWidth = toFloat <| Pixels.inPixels (Tuple.first view3dDimensions)

view3dHeight = toFloat <| Pixels.inPixels (Tuple.second view3dDimensions)

showDecimal2 x =
    let
        locale =
            { usLocale | decimals = Exact 2 }
    in
    format locale x


showDecimal6 x =
    let
        locale =
            { usLocale | decimals = Exact 6 }
    in
    format locale x
