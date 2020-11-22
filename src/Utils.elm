module Utils exposing (..)

import Color
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
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



gradientColour slope =
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


incrementMaybeModulo modulo mx =
    Maybe.map (\x -> modBy modulo (x + 1)) mx


decrementMaybeModulo modulo mx =
    Maybe.map (\x -> modBy modulo (x - 1)) mx
