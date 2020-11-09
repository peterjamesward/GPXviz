module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , trackPoints = []
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
            ( { model
                | gpx = Just content
                , trackPoints = parseTrackPoints content
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
    case model.gpx of
        Nothing ->
            button [ onClick GpxRequested ] [ text "Load GPX" ]

        Just _ ->
            div [] <|
                List.map viewTrackPoint model.trackPoints


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
