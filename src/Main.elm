module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, hr, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (minimum)
import Maybe.Extra
import Regex
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias TrackPoint =
    { lat : Float
    , lon : Float
    , ele : Float
    }


type alias Model =
    { gpx : Maybe String
    , trackPoints : List TrackPoint
    , minimums : TrackPoint
    , maximums : TrackPoint
    }


zerotp =
    TrackPoint 0.0 0.0 0.0


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , trackPoints = []
      , minimums = zerotp
      , maximums = zerotp
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        lowerBounds tps =
            { lat = Maybe.withDefault 0.0 <| List.minimum <| List.map .lat tps
            , lon = Maybe.withDefault 0.0 <| List.minimum <| List.map .lon tps
            , ele = Maybe.withDefault 0.0 <| List.minimum <| List.map .ele tps
            }

        upperBounds tps =
            { lat = Maybe.withDefault 0.0 <| List.maximum <| List.map .lat tps
            , lon = Maybe.withDefault 0.0 <| List.maximum <| List.map .lon tps
            , ele = Maybe.withDefault 0.0 <| List.maximum <| List.map .ele tps
            }
    in
    case msg of
        GpxRequested ->
            ( model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( model
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            ( let
                tps =
                    parseTrackPoints content
              in
              { model
                | gpx = Just content
                , trackPoints = tps
                , minimums = lowerBounds tps
                , maximums = upperBounds tps
              }
            , Cmd.none
            )


parseTrackPoints : String -> List TrackPoint
parseTrackPoints xml =
    let
        reg t =
            Maybe.withDefault Regex.never <| Regex.fromString t

        latitudes =
            Regex.find (reg "lat=\\\"([\\d\\.-]*)\\\"") xml |> matches

        longitudes =
            Regex.find (reg "lon=\\\"([\\d\\.-]*)\\\"") xml |> matches

        elevations =
            Regex.find (reg "<ele>([\\d\\.-]*)</ele>") xml |> matches

        makeTrackPoint mayLat mayLon mayEle =
            case ( mayLat, mayLon, mayEle ) of
                ( Just a, Just b, Just c ) ->
                    Just
                        { lat = a
                        , lon = b
                        , ele = c
                        }

                _ ->
                    Nothing

        matches xs =
            List.map value xs

        value x =
            case x.submatches of
                (Just val) :: _ ->
                    String.toFloat val

                _ ->
                    Nothing
    in
    List.map3
        makeTrackPoint
        latitudes
        longitudes
        elevations
        |> Maybe.Extra.values



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GpxRequested ] [ text "Load GPX" ]
        , case model.gpx of
            Nothing ->
                hr [] []

            Just _ ->
                div []
                    [ viewTrackPoint model.minimums
                    , viewTrackPoint model.maximums
                    ]
        ]


viewTrackPoint : TrackPoint -> Html Msg
viewTrackPoint trkpnt =
    p [ style "white-space" "pre" ]
        [ text <| " Lat:" ++ String.fromFloat trkpnt.lat
        , text <| " Lon:" ++ String.fromFloat trkpnt.lon
        , text <| " Ele:" ++ String.fromFloat trkpnt.ele
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
