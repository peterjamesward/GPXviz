module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, hr, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List
import Maybe.Extra
import Regex
import Task



--TODO: Use elm-ui
--TODO: Optional tabular display of track points
--TODO: Track points in elm-3d-scene
--TODO: Create road segments
--TODO: Road segments in elm-3d-scene
--TODO: Colour by gradient
--TODO: Camera rotation
--TODO: Fly-through
--TODO: Toggle display elements
--TODO: Dark mode
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
    -- This is the basic info we extract from a GPX file.
    { lat : Float
    , lon : Float
    , ele : Float
    }


type alias DrawingNode =
    -- We will draw in a rectangular space using metre units. Probably.
    { trackPoint : TrackPoint
    , northOffset : Float -- metres from bottom edge of bounding box
    , eastOffset : Float -- metres from left edge of bounding box
    , vertOffset : Float -- metres from base of bounding box
    }


type alias Model =
    { gpx : Maybe String
    , trackPoints : List TrackPoint
    , minimums : TrackPoint
    , maximums : TrackPoint
    , nodes : List DrawingNode
    }


zerotp =
    TrackPoint 0.0 0.0 0.0


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , trackPoints = []
      , minimums = zerotp
      , maximums = zerotp
      , nodes = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String


metresPerDegreeLongitude =
    78846.81


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

        prepareDrawingNode mins maxs tp =
            { trackPoint = tp
            , northOffset = (tp.lat - mins.lat) * metresPerDegreeLongitude
            , eastOffset = (tp.lon - mins.lon) * metresPerDegreeLongitude * cos tp.lat
            , vertOffset = tp.ele - mins.ele
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

                mins =
                    lowerBounds tps

                maxs =
                    upperBounds tps
              in
              { model
                | gpx = Just content
                , trackPoints = tps
                , minimums = mins
                , maximums = maxs
                , nodes = List.map (prepareDrawingNode mins maxs) tps
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
