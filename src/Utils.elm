module Utils exposing (..)

import Color
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Regex


type alias Point =
    ( Float, Float )


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
