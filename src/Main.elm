module Main exposing (main)

import Angle
import Browser
import Camera3d
import Color
import Direction3d
import Element exposing (Element, column, fill, focused, html, htmlAttribute, layout, mouseOver, none, padding, paragraph, rgb255, row, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import Html.Attributes exposing (style)
import Length
import List
import Maybe.Extra
import Pixels
import Point3d
import Regex
import Scene3d
import Scene3d.Material as Material
import Task
import Viewpoint3d



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
    Browser.document
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
    , x : Float -- east offset convrted to [-1, +1] system
    , y : Float -- north, ditto
    , z : Float -- vert, ditto
    }


type alias Model =
    { gpx : Maybe String
    , trackPoints : List TrackPoint
    , minimums : TrackPoint
    , maximums : TrackPoint
    , centres : TrackPoint -- simplify converting to [-1, +1] coords
    , largestDimension : Float -- biggest bounding box edge determines scaling factor
    , nodes : List DrawingNode
    , trackName : Maybe String
    }


zerotp =
    TrackPoint 0.0 0.0 0.0


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , trackPoints = []
      , minimums = zerotp
      , maximums = zerotp
      , centres = zerotp
      , largestDimension = 1.0
      , nodes = []
      , trackName = Nothing
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

                findCentres =
                    { lat = (mins.lat + maxs.lat) / 2.0
                    , lon = (mins.lon + maxs.lon) / 2.0
                    , ele = (mins.ele + maxs.ele) / 2.0
                    }

                scalingFactor =
                    max (maxs.lat - mins.lat) (maxs.lon - mins.lon)

                prepareDrawingNode tp =
                    { trackPoint = tp
                    , northOffset = (tp.lat - mins.lat) * metresPerDegreeLongitude
                    , eastOffset = (tp.lon - mins.lon) * metresPerDegreeLongitude * cos tp.lat
                    , vertOffset = tp.ele - mins.ele
                    , x = (tp.lon - findCentres.lon) / (0.5 * scalingFactor)
                    , y = (tp.lat - findCentres.lat) / (0.5 * scalingFactor)
                    , z = (tp.ele - findCentres.ele) / (0.5 * scalingFactor * metresPerDegreeLongitude)
                    }
              in
              { model
                | gpx = Just content
                , trackPoints = tps
                , minimums = mins
                , maximums = maxs
                , centres = findCentres
                , largestDimension = scalingFactor
                , nodes = List.map prepareDrawingNode tps
                , trackName = parseTrackName content
              }
            , Cmd.none
            )


reg t =
    -- Helper to make a regex pattern.
    Maybe.withDefault Regex.never <| Regex.fromString t


parseTrackPoints : String -> List TrackPoint
parseTrackPoints xml =
    let
        latitudes =
            Regex.find (reg "lat=\\\"([\\d\\.-]*)\\\"") xml |> matches

        longitudes =
            Regex.find (reg "lon=\\\"([\\d\\.-]*)\\\"") xml |> matches

        elevations =
            Regex.find (reg "<ele>([\\d\\.-]*)<\\/ele>") xml |> matches

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


parseTrackName xml =
    case Regex.find (reg "<name>(.*)<\\/name>") xml of
        [] ->
            Nothing

        x :: _ ->
            case x.submatches of
                [] ->
                    Nothing

                n :: _ ->
                    n



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "GPX viewer"
    , body =
        [ layout
            [ width fill
            , padding 20
            , spacing 10
            , htmlAttribute <| style "touch-action" "manipulation"
            ]
          <|
            column []
                [ button
                    prettyButtonStyles
                    { onPress = Just GpxRequested
                    , label = text "Load GPX"
                    }
                , case model.gpx of
                    Nothing ->
                        none

                    Just _ ->
                        column []
                            [ displayName model.trackName
                            , viewPointCloud model.nodes
                            , viewBoundingBox model
                            , viewTrackPointTable model
                            ]
                ]
        ]
    }


viewPointCloud nodes =
    let
        points =
            List.map
                (\node ->
                    Point3d.meters
                        node.x
                        node.y
                        node.z
                )
                nodes

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        pointEntities =
            points
                |> List.map
                    (\point ->
                        Scene3d.point { radius = Pixels.float 5 }
                            (Material.color Color.blue)
                            point
                    )

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 2 6 4
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    html <|
        Scene3d.unlit
            { camera = camera
            , dimensions = ( Pixels.int 500, Pixels.int 500 )
            , background = Scene3d.transparentBackground
            , clipDepth = Length.meters 1.0
            , entities = pointEntities
            }


viewBoundingBox model =
    row []
        [ viewTrackPoint model.minimums
        , viewTrackPoint model.maximums
        ]


viewTrackPointTable model =
    table []
        { data = model.nodes
        , columns =
            [ { header = text "Latitude"
              , width = fill
              , view = \node -> text <| String.fromFloat node.trackPoint.lat
              }
            , { header = text "x"
              , width = fill
              , view = \node -> text <| String.fromFloat node.x
              }
            , { header = text "Longitude"
              , width = fill
              , view = \node -> text <| String.fromFloat node.trackPoint.lon
              }
            , { header = text "y"
              , width = fill
              , view = \node -> text <| String.fromFloat node.y
              }
            , { header = text "Elevation"
              , width = fill
              , view = \node -> text <| String.fromFloat node.trackPoint.ele
              }
            , { header = text "z"
              , width = fill
              , view = \node -> text <| String.fromFloat node.z
              }
            ]
        }


displayName n =
    case n of
        Just s ->
            text s

        _ ->
            none


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


viewTrackPoint : TrackPoint -> Element Msg
viewTrackPoint trkpnt =
    column [ padding 5, spacing 5 ]
        [ text <| "Lat:" ++ String.fromFloat trkpnt.lat
        , text <| "Lon:" ++ String.fromFloat trkpnt.lon
        , text <| "Ele:" ++ String.fromFloat trkpnt.ele
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
