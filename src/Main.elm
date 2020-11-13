module Main exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Camera3d
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeZ, positiveZ)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (color)
import Element.Font as Font
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Select as Select
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (meters)
import List exposing (tail)
import Maybe.Extra
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Regex
import Scene3d exposing (Entity, cone, cylinder)
import Scene3d.Material as Material
import Spherical exposing (range)
import Task
import Viewpoint3d



--TODO: View mode -- choose focal node, mouse rotates scene, move in/out, change focal length. (see which works better.)
--TODO: Toggle display elements.
--TODO: Detect abrupt gradient changes.
--TODO: Gradient colours (optional).


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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


type alias DrawingRoad =
    { startsAt : DrawingNode
    , endsAt : DrawingNode
    , length : Float
    , bearing : Float
    , gradient : Float -- radians
    , startDistance : Float
    , endDistance : Float
    , index : Int
    }


type ButtonPosition
    = First
    | Mid
    | Last


type MyCoord
    = SomeCoord


type ViewingMode
    = PointCloud
    | RollerCoaster


type alias Model =
    { gpx : Maybe String
    , gpxUrl : String
    , trackPoints : List TrackPoint
    , minimums : TrackPoint
    , maximums : TrackPoint
    , centres : TrackPoint -- simplify converting to [-1, +1] coords
    , largestDimension : Float -- biggest bounding box edge determines scaling factor
    , nodes : List DrawingNode
    , roads : List DrawingRoad
    , trackName : Maybe String
    , azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    , entities : List (Entity MyCoord)
    , httpError : Maybe String
    , currentSegment : Int
    , metresToClipSpace : Float -- Probably should be a proper metric tag!
    , viewingMode : ViewingMode
    , summary : Maybe SummaryData
    , nodeArray : Array DrawingNode
    , roadArray : Array DrawingRoad
    }


type alias SummaryData =
    { highestMetres : Float
    , lowestMetres : Float
    , trackLength : Float
    , climbingDistance : Float
    , descendingDistance : Float
    , totalClimbing : Float
    , totalDescending : Float
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | UserMovedSlider Int
    | BackOne
    | ForwardOne
    | ChooseViewMode ViewingMode


zerotp =
    TrackPoint 0.0 0.0 0.0


percentInOneRadian =
    -- Who knew?
    15.91549430919


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , gpxUrl = ""
      , trackPoints = []
      , minimums = zerotp
      , maximums = zerotp
      , centres = zerotp
      , largestDimension = 1.0
      , nodes = []
      , roads = []
      , trackName = Nothing
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , entities = []
      , httpError = Nothing
      , currentSegment = 1
      , metresToClipSpace = 1.0
      , viewingMode = PointCloud
      , summary = Nothing
      , nodeArray = Array.empty
      , roadArray = Array.empty
      }
    , Cmd.none
    )


metresPerDegreeLongitude =
    78846.81


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
            ( parseGPXintoModel content model
            , Cmd.none
            )

        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        UserMovedSlider segment ->
            ( { model | currentSegment = segment }, Cmd.none )

        ForwardOne ->
            ( { model
                | currentSegment =
                    min (model.currentSegment + 1)
                        (List.length model.roads)
              }
            , Cmd.none
            )

        BackOne ->
            ( { model
                | currentSegment =
                    max (model.currentSegment - 1)
                        1
              }
            , Cmd.none
            )

        ChooseViewMode mode ->
            ( { model | viewingMode = mode }
            , Cmd.none
            )


parseGPXintoModel content model =
    let
        lowerBounds tp =
            { lat = Maybe.withDefault 0.0 <| List.minimum <| List.map .lat tp
            , lon = Maybe.withDefault 0.0 <| List.minimum <| List.map .lon tp
            , ele = Maybe.withDefault 0.0 <| List.minimum <| List.map .ele tp
            }

        upperBounds tp =
            { lat = Maybe.withDefault 0.0 <| List.maximum <| List.map .lat tp
            , lon = Maybe.withDefault 0.0 <| List.maximum <| List.map .lon tp
            , ele = Maybe.withDefault 0.0 <| List.maximum <| List.map .ele tp
            }

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

        metresToClipSpace =
            1 / (0.5 * scalingFactor * metresPerDegreeLongitude)

        elevationToClipSpace e =
            (e - findCentres.ele) * metresToClipSpace

        prepareDrawingNode tp =
            { trackPoint = tp
            , northOffset = (tp.lat - mins.lat) * metresPerDegreeLongitude
            , eastOffset = (tp.lon - mins.lon) * metresPerDegreeLongitude * cos tp.lat
            , vertOffset = tp.ele - mins.ele
            , x = (tp.lon - findCentres.lon) / (0.5 * scalingFactor)
            , y = (tp.lat - findCentres.lat) / (0.5 * scalingFactor)
            , z = elevationToClipSpace tp.ele
            }

        drawingNodes =
            List.map prepareDrawingNode tps

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        pointEntities =
            List.map
                (\node ->
                    cylinder (Material.color Color.brown) <|
                        Cylinder3d.startingAt
                            (Point3d.meters node.x node.y (node.z - 1.0 * metresToClipSpace))
                            negativeZ
                            { radius = meters <| 1.0 * metresToClipSpace
                            , length = meters <| (node.trackPoint.ele - 1.0) * metresToClipSpace
                            }
                )
                drawingNodes
                ++ List.map
                    (\node ->
                        cone (Material.color Color.black) <|
                            Cone3d.startingAt
                                (Point3d.meters node.x node.y (node.z - 1.0 * metresToClipSpace))
                                positiveZ
                                { radius = meters <| 1.0 * metresToClipSpace
                                , length = meters <| 1.0 * metresToClipSpace
                                }
                    )
                    drawingNodes

        seaLevel =
            Scene3d.quad (Material.color Color.green)
                (Point3d.meters -1.2 -1.2 (elevationToClipSpace 0.0))
                (Point3d.meters 1.2 -1.2 (elevationToClipSpace 0.0))
                (Point3d.meters 1.2 1.2 (elevationToClipSpace 0.0))
                (Point3d.meters -1.2 1.2 (elevationToClipSpace 0.0))

        roadEntities =
            List.concat <|
                List.map roadEntity roadSegments

        roadEntity segment =
            let
                kerbX =
                    -- Road is assumed to be 6 m wide.
                    3.0 * cos segment.bearing * metresToClipSpace

                kerbY =
                    3.0 * sin segment.bearing * metresToClipSpace

                edgeHeight =
                    -- Let's try a low wall at the road's edges.
                    0.3 * metresToClipSpace
            in
            [ --surface
              Scene3d.quad (Material.color Color.grey)
                (Point3d.meters (segment.startsAt.x + kerbX) (segment.startsAt.y - kerbY) segment.startsAt.z)
                (Point3d.meters (segment.endsAt.x + kerbX) (segment.endsAt.y - kerbY) segment.endsAt.z)
                (Point3d.meters (segment.endsAt.x - kerbX) (segment.endsAt.y + kerbY) segment.endsAt.z)
                (Point3d.meters (segment.startsAt.x - kerbX) (segment.startsAt.y + kerbY) segment.startsAt.z)

            -- kerb walls
            , Scene3d.quad (Material.color Color.darkGrey)
                (Point3d.meters (segment.startsAt.x + kerbX) (segment.startsAt.y - kerbY) segment.startsAt.z)
                (Point3d.meters (segment.endsAt.x + kerbX) (segment.endsAt.y - kerbY) segment.endsAt.z)
                (Point3d.meters (segment.endsAt.x + kerbX) (segment.endsAt.y - kerbY) (segment.endsAt.z + edgeHeight))
                (Point3d.meters (segment.startsAt.x + kerbX) (segment.startsAt.y - kerbY) (segment.startsAt.z + edgeHeight))
            , Scene3d.quad (Material.color Color.darkGrey)
                (Point3d.meters (segment.startsAt.x - kerbX) (segment.startsAt.y + kerbY) segment.startsAt.z)
                (Point3d.meters (segment.endsAt.x - kerbX) (segment.endsAt.y + kerbY) segment.endsAt.z)
                (Point3d.meters (segment.endsAt.x - kerbX) (segment.endsAt.y + kerbY) (segment.endsAt.z + edgeHeight))
                (Point3d.meters (segment.startsAt.x - kerbX) (segment.startsAt.y + kerbY) (segment.startsAt.z + edgeHeight))
            ]

        roadSegments =
            List.map3 roadSegment
                drawingNodes
                (Maybe.withDefault [] <| tail drawingNodes)
                (List.range 1 (List.length drawingNodes))

        roadSegment node1 node2 index =
            let
                xDifference =
                    node2.eastOffset - node1.eastOffset

                yDifference =
                    node2.northOffset - node1.northOffset

                zDifference =
                    node2.trackPoint.ele - node1.trackPoint.ele

                earthDistance =
                    -- Great circle distance (!) ignoring elevation difference
                    Spherical.range ( degrees node2.trackPoint.lat, degrees node2.trackPoint.lon )
                        ( degrees node1.trackPoint.lat, degrees node1.trackPoint.lon )

                segmentLength =
                    sqrt <| yDifference * yDifference + xDifference * xDifference + zDifference * zDifference
            in
            { startsAt = node1
            , endsAt = node2
            , length = earthDistance
            , bearing =
                Spherical.findBearingToTarget
                    ( degrees node1.trackPoint.lat, degrees node1.trackPoint.lon )
                    ( degrees node2.trackPoint.lat, degrees node2.trackPoint.lon )
            , gradient =
                if earthDistance > 0 then
                    100.0 * (zDifference / earthDistance)

                else
                    0.0
            , startDistance = 0.0
            , endDistance = 0.0
            , index = index
            }

        accumulateInfo segment summary =
            { trackLength = summary.trackLength + segment.length
            , highestMetres =
                max summary.highestMetres <|
                    max segment.startsAt.trackPoint.ele segment.endsAt.trackPoint.ele
            , lowestMetres =
                min summary.lowestMetres <|
                    min segment.startsAt.trackPoint.ele segment.endsAt.trackPoint.ele
            , climbingDistance =
                if segment.gradient > 0 then
                    summary.climbingDistance + segment.length

                else
                    summary.climbingDistance
            , descendingDistance =
                if segment.gradient < 0 then
                    summary.climbingDistance + segment.length

                else
                    summary.climbingDistance
            , totalClimbing =
                if segment.gradient > 0 then
                    summary.totalClimbing + segment.endsAt.trackPoint.ele - segment.startsAt.trackPoint.ele

                else
                    summary.totalClimbing
            , totalDescending =
                if segment.gradient < 0 then
                    summary.totalClimbing - segment.endsAt.trackPoint.ele + segment.startsAt.trackPoint.ele

                else
                    summary.totalClimbing
            }

        summarise =
            List.foldl accumulateInfo
                { trackLength = 0.0
                , highestMetres = -9999.9
                , lowestMetres = 9999.9
                , climbingDistance = 0.0
                , descendingDistance = 0.0
                , totalClimbing = 0.0
                , totalDescending = 0.0
                }
                roadSegments
    in
    { model
        | gpx = Just content
        , trackPoints = tps
        , minimums = mins
        , maximums = maxs
        , centres = findCentres
        , largestDimension = scalingFactor
        , nodes = drawingNodes
        , roads = roadSegments
        , trackName = parseTrackName content
        , entities = seaLevel :: pointEntities ++ roadEntities
        , metresToClipSpace = metresToClipSpace
        , currentSegment = 1
        , summary = Just summarise
        , nodeArray = Array.fromList drawingNodes
        , roadArray = Array.fromList roadSegments
    }


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


decimalPrecision =
    3


showDecimal x =
    let
        locale =
            { usLocale | decimals = Exact 2 }
    in
    format locale x


view : Model -> Browser.Document Msg
view model =
    let
        viewModeChoices =
            Input.radioRow
                [ Border.rounded 6
                , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
                ]
                { onChange = ChooseViewMode
                , selected = Just model.viewingMode
                , label =
                    Input.labelHidden "Choose view"
                , options =
                    [ Input.optionWith PointCloud <| radioButton First "Whole route"
                    , Input.optionWith RollerCoaster <| radioButton Last "First person"
                    ]
                }
    in
    { title = "GPX viewer"
    , body =
        [ layout
            [ width fill
            , padding 20
            , spacing 20
            ]
          <|
            column
                [ spacing 20 ]
                [ button
                    prettyButtonStyles
                    { onPress = Just GpxRequested
                    , label = text "Load GPX from your computer"
                    }
                , case model.gpx of
                    Nothing ->
                        none

                    Just _ ->
                        column []
                            [ viewModeChoices
                            , displayName model.trackName
                            , view3D model
                            ]
                ]
        ]
    }


view3D model =
    case model.viewingMode of
        PointCloud ->
            viewPointCloud model

        RollerCoaster ->
            viewRollerCoasterTrackAndControls model


viewPointCloud model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0.0 0.0 0.0
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 4
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        showSummary =
            case model.summary of
                Just summary ->
                    row []
                        [ column [ spacing 10 ]
                            [ text "Highest point "
                            , text "Lowest point "
                            , text "Track length "
                            , text "Climbing distance "
                            , text "Elevation gain "
                            , text "Descending distance "
                            , text "Elevation loss "
                            ]
                        , column [ spacing 10, alignRight ]
                            [ text <| showDecimal summary.highestMetres
                            , text <| showDecimal summary.lowestMetres
                            , text <| showDecimal summary.trackLength
                            , text <| showDecimal summary.climbingDistance
                            , text <| showDecimal summary.totalClimbing
                            , text <| showDecimal summary.descendingDistance
                            , text <| showDecimal summary.totalDescending
                            ]
                        ]

                _ ->
                    none
    in
    row []
        [ el
            [ pointer
            , htmlAttribute <| style "touch-action" "manipulation"
            ]
          <|
            html <|
                Scene3d.unlit
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.transparentBackground
                    , clipDepth = Length.meters 1.0
                    , entities = model.entities
                    }
        , showSummary
        ]


toDegrees rads =
    rads * 180.0 / pi


viewRollerCoasterTrackAndControls model =
    let
        slider =
            Input.slider
                [ height <| px 80
                , width <| px 500
                , centerY
                , behindContent <|
                    -- Slider track
                    el
                        [ width <| px 500
                        , height <| px 30
                        , centerY
                        , centerX
                        , Background.color <| rgb255 114 159 207
                        , Border.rounded 6
                        ]
                        Element.none
                ]
                { onChange = UserMovedSlider << round
                , label =
                    Input.labelBelow [] <|
                        text "Drag slider or use arrow buttons"
                , min = 1.0
                , max = toFloat <| (List.length model.roads) - 1
                , step = Just 1
                , value = toFloat model.currentSegment
                , thumb = Input.defaultThumb
                }

        getRoad : Maybe DrawingRoad
        getRoad =
            Array.get model.currentSegment model.roadArray

        controls =
            row
                [ centerX, spaceEvenly, centerY ]
                [ slider
                , button
                    prettyButtonStyles
                    { onPress = Just BackOne
                    , label = text "◀︎"
                    }
                , button
                    prettyButtonStyles
                    { onPress = Just ForwardOne
                    , label = text "►︎"
                    }
                ]
    in
    case getRoad of
        Nothing ->
            none

        Just road ->
            row []
                [ column
                    [ width <| px 900
                    , spacing 10
                    ]
                    [ viewRoadSegment model road
                    , controls
                    ]
                , row []
                    [ column [ spacing 10 ]
                        [ text "Start point index "
                        , text "Start latitude "
                        , text "Start longitude "
                        , text "Start elevation "
                        , text "End latitude "
                        , text "End longitude "
                        , text "End elevation "
                        , text "Length "
                        , text "Gradient "
                        , text "Bearing "
                        ]
                    , column [ spacing 10 ]
                        [ text <| String.fromInt model.currentSegment
                        , text <| showDecimal road.startsAt.trackPoint.lat
                        , text <| showDecimal road.startsAt.trackPoint.lon
                        , text <| showDecimal road.startsAt.trackPoint.ele
                        , text <| showDecimal road.endsAt.trackPoint.lat
                        , text <| showDecimal road.endsAt.trackPoint.lon
                        , text <| showDecimal road.endsAt.trackPoint.ele
                        , text <| showDecimal road.length
                        , text <| showDecimal road.gradient
                        , text <|
                            showDecimal <|
                                toDegrees <|
                                    if road.bearing < 0 then
                                        pi + pi + road.bearing

                                    else
                                        road.bearing
                        ]
                    ]
                ]


viewRoadSegment model road =
    let
        eyeHeight =
            -- Helps to be higher up.
            2.5 * model.metresToClipSpace

        cameraSetback =
            1.0 * model.metresToClipSpace

        cameraViewpoint someTarmac =
            Viewpoint3d.lookAt
                { eyePoint =
                    Point3d.meters
                        (someTarmac.startsAt.x - cameraSetback * sin road.bearing)
                        (someTarmac.startsAt.y - cameraSetback * cos road.bearing)
                        (someTarmac.startsAt.z + eyeHeight)
                , focalPoint = Point3d.meters someTarmac.endsAt.x someTarmac.endsAt.y someTarmac.endsAt.z
                , upDirection = Direction3d.positiveZ
                }

        camera someTarmac =
            Camera3d.perspective
                { viewpoint = cameraViewpoint someTarmac
                , verticalFieldOfView = Angle.degrees 80
                }
    in
    html <|
        Scene3d.sunny
            { camera = camera road
            , dimensions = ( Pixels.int 800, Pixels.int 500 )
            , background = Scene3d.backgroundColor Color.lightBlue
            , clipDepth = Length.meters (1.0 * model.metresToClipSpace)
            , entities = model.entities
            , upDirection = positiveZ
            , sunlightDirection = negativeZ
            , shadows = True
            }


displayName n =
    case n of
        Just s ->
            el [ Font.size 32, padding 8 ]
                (text s)

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


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


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
        [ paddingEach { left = 20, right = 20, top = 10, bottom = 10 }
        , Border.roundEach corners
        , Border.widthEach borders
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        , Background.color <|
            if state == Input.Selected then
                rgb255 0xFF 0xFF 0xFF

            else
                rgb255 114 159 207
        ]
    <|
        el [ centerX, centerY ] <|
            text label
