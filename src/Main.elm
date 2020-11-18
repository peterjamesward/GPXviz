module Main exposing (main)

import Angle exposing (Angle, inDegrees, normalize)
import Array exposing (Array)
import Browser
import Camera3d
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeX, negativeZ, positiveZ)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (color)
import Element.Font as Font
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Geometry101 as G exposing (..)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Iso8601
import Length exposing (meters)
import List exposing (tail)
import Markdown exposing (defaultOptions)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point3d
import Regex
import Scene3d exposing (Entity, cone, cylinder)
import Scene3d.Material as Material
import Spherical exposing (range)
import Task
import Time
import TrackPoint exposing (TrackPoint)
import Viewpoint3d
import WriteGPX exposing (writeGPX)



--TODO: Single, more prominent Save button (same level as mode selection)
--TODO: Autofix sharp bends with circular arcs.
-- 1. Preview proposed fix in 3rd person view.
--TODO: Constant speed flythrough (works in either view mode, can still rotate).
--TODO: Optional road shadow.
--TODO: Optional plain green vertical instead of rainbow.
--TODO: Always autofix the zero length segments.
--TODO: Some way to join the start and end of a loop (although MR does this).


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = viewGenericNew
        , update = update
        , subscriptions = subscriptions
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


type alias AbruptChange =
    { node : DrawingNode
    , before : DrawingRoad
    , after : DrawingRoad
    }


type alias UndoEntry =
    { label : String
    , trackPoints : List TrackPoint
    }


type ButtonPosition
    = First
    | Mid
    | Last


type MyCoord
    = SomeCoord


type ViewingMode
    = OverviewView
    | FirstPersonView
    | ThirdPersonView
    | OptionsView
    | ProblemsView
    | InputErrorView


type ProblemType
    = ZeroLengthSegment
    | AbruptGradientChanges
    | SharpBends


type ThirdPersonSubmode
    = ShowData
    | ShowGradientFixes
    | ShowBendFixes


type alias DisplayOptions =
    { roadPillars : Bool
    , roadCones : Bool
    , roadTrack : Bool
    , gradientFill : Bool
    , problems : Bool
    }


defaultDisplayOptions =
    { roadPillars = True
    , roadCones = True
    , roadTrack = True
    , gradientFill = True
    , problems = False
    }


type alias Model =
    { gpx : Maybe String
    , filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , gpxUrl : String
    , trackPoints : List TrackPoint
    , largestDimension : Float -- biggest bounding box edge determines scaling factor
    , seaLevelInClipSpace : Float
    , nodes : List DrawingNode
    , roads : List DrawingRoad
    , trackName : Maybe String
    , azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Maybe Point -- Capture mouse down position (when clicking on the 3D control)
    , entities : List (Entity MyCoord)
    , httpError : Maybe String
    , currentNode : Maybe Int
    , markedNode : Maybe Int
    , metresToClipSpace : Float -- Probably should be a proper metric tag!
    , viewingMode : ViewingMode
    , summary : Maybe SummaryData
    , nodeArray : Array DrawingNode
    , roadArray : Array DrawingRoad
    , zoomLevelOverview : Float
    , zoomLevelFirstPerson : Float
    , zoomLevelThirdPerson : Float
    , displayOptions : DisplayOptions
    , abruptGradientChanges : List AbruptChange -- change in gradient exceeds user's threshold
    , abruptBearingChanges : List AbruptChange -- change in gradient exceeds user's threshold
    , zeroLengths : List DrawingRoad
    , gradientChangeThreshold : Float
    , bearingChangeThreshold : Int
    , selectedProblemType : ProblemType
    , hasBeenChanged : Bool
    , smoothingEndIndex : Maybe Int
    , undoStack : List UndoEntry
    , thirdPersonSubmode : ThirdPersonSubmode
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


type alias Point =
    ( Float, Float )


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | UserMovedNodeSlider Int
    | PositionBackOne
    | PositionForwardOne
    | ChooseViewMode ViewingMode
    | ZoomLevelOverview Float
    | ZoomLevelFirstPerson Float
    | ZoomLevelThirdPerson Float
    | ImageGrab Point
    | ImageRotate Point
    | ImageRelease Point
    | ToggleRoad Bool
    | ToggleGradient Bool
    | TogglePillars Bool
    | ToggleCones Bool
    | SetGradientChangeThreshold Float
    | SetBearingChangeThreshold Float
    | SelectProblemType ProblemType
    | DeleteZeroLengthSegments
    | OutputGPX
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SetSmoothingEnd Int
    | SmoothGradient Int Int Float
    | SetThirdPersonSubmode ThirdPersonSubmode
    | Undo
    | ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , gpxUrl = ""
      , trackPoints = []
      , largestDimension = 1.0
      , seaLevelInClipSpace = 0.0
      , nodes = []
      , roads = []
      , trackName = Nothing
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = Nothing
      , entities = []
      , httpError = Nothing
      , currentNode = Nothing
      , markedNode = Nothing
      , metresToClipSpace = 1.0
      , viewingMode = OptionsView
      , summary = Nothing
      , nodeArray = Array.empty
      , roadArray = Array.empty
      , zoomLevelOverview = 1.0
      , zoomLevelFirstPerson = 1.0
      , zoomLevelThirdPerson = 1.0
      , displayOptions = defaultDisplayOptions
      , abruptGradientChanges = []
      , abruptBearingChanges = []
      , zeroLengths = []
      , gradientChangeThreshold = 10.0 -- Note, this is not an angle, it's a percentage (tangent).
      , bearingChangeThreshold = 90
      , selectedProblemType = ZeroLengthSegment
      , hasBeenChanged = False
      , smoothingEndIndex = Nothing
      , undoStack = []
      , thirdPersonSubmode = ShowData
      }
    , Task.perform AdjustTimeZone Time.here
    )


metresPerDegreeLongitude =
    78846.81


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        options =
            model.displayOptions
    in
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        GpxRequested ->
            ( model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( { model | filename = Just (File.name file) }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            ( parseGPXintoModel content model
                |> deriveNodesAndRoads
                |> deriveVisualEntities
                |> deriveProblems
                |> resetViewSettings
            , Cmd.none
            )

        UserMovedNodeSlider node ->
            ( { model | currentNode = Just node }
                |> deriveVisualEntities
            , Cmd.none
            )

        SetSmoothingEnd idx ->
            ( { model | smoothingEndIndex = Just idx }
            , Cmd.none
            )

        PositionForwardOne ->
            ( { model
                | currentNode = incrementMaybeModulo (List.length model.roads - 1) model.currentNode
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        PositionBackOne ->
            ( { model
                | currentNode = decrementMaybeModulo (List.length model.roads - 1) model.currentNode
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        MarkerForwardOne ->
            ( { model
                | markedNode = incrementMaybeModulo (List.length model.roads - 1) model.markedNode
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        MarkerBackOne ->
            ( { model
                | markedNode = decrementMaybeModulo (List.length model.roads - 1) model.markedNode
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        ChooseViewMode mode ->
            ( { model | viewingMode = mode }
                |> deriveVisualEntities
            , Cmd.none
            )

        ZoomLevelOverview level ->
            ( { model | zoomLevelOverview = level }
            , Cmd.none
            )

        ZoomLevelFirstPerson level ->
            ( { model | zoomLevelFirstPerson = level }
            , Cmd.none
            )

        ZoomLevelThirdPerson level ->
            ( { model | zoomLevelThirdPerson = level }
            , Cmd.none
            )

        ImageGrab ( dx, dy ) ->
            ( { model | orbiting = Just ( dx, dy ) }
            , Cmd.none
            )

        ImageRotate ( dx, dy ) ->
            case model.orbiting of
                Just ( startX, startY ) ->
                    let
                        newAzimuth =
                            Angle.degrees <|
                                inDegrees model.azimuth
                                    - (dx - startX)

                        newElevation =
                            Angle.degrees <|
                                inDegrees model.elevation
                                    + (dy - startY)
                    in
                    ( { model
                        | azimuth = newAzimuth
                        , elevation = newElevation
                        , orbiting = Just ( dx, dy )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ImageRelease _ ->
            ( { model | orbiting = Nothing }
            , Cmd.none
            )

        ToggleCones _ ->
            ( { model
                | displayOptions = { options | roadCones = not options.roadCones }
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        TogglePillars _ ->
            ( { model
                | displayOptions = { options | roadPillars = not options.roadPillars }
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        ToggleRoad _ ->
            ( { model
                | displayOptions = { options | roadTrack = not options.roadTrack }
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        ToggleGradient _ ->
            ( { model
                | displayOptions = { options | gradientFill = not options.gradientFill }
              }
                |> deriveVisualEntities
            , Cmd.none
            )

        SetGradientChangeThreshold threshold ->
            ( { model
                | gradientChangeThreshold = threshold
              }
                |> deriveProblems
            , Cmd.none
            )

        SetBearingChangeThreshold threshold ->
            ( { model
                | bearingChangeThreshold = round threshold
              }
                |> deriveProblems
            , Cmd.none
            )

        SelectProblemType prob ->
            ( { model
                | selectedProblemType = prob
              }
            , Cmd.none
            )

        DeleteZeroLengthSegments ->
            ( deleteZeroLengthSegments model
                |> deriveNodesAndRoads
                |> deriveVisualEntities
                |> deriveProblems
            , Cmd.none
            )

        OutputGPX ->
            ( { model | hasBeenChanged = False }
            , outputGPX model
            )

        SmoothGradient s f g ->
            ( smoothGradient model s f g
                |> deriveNodesAndRoads
                |> deriveVisualEntities
                |> deriveProblems
            , Cmd.none
            )

        SetThirdPersonSubmode mode ->
            ( { model | thirdPersonSubmode = mode }
                |> deriveNodesAndRoads
                |> deriveVisualEntities
            , Cmd.none
            )

        Undo ->
            ( case model.undoStack of
                action :: undos ->
                    { model
                        | trackPoints = action.trackPoints
                        , undoStack = undos
                    }
                        |> deriveNodesAndRoads
                        |> deriveVisualEntities
                        |> deriveProblems

                _ ->
                    model
            , Cmd.none
            )

        ToggleMarker ->
            ( { model
                | markedNode =
                    case model.markedNode of
                        Just _ ->
                            Nothing

                        Nothing ->
                            model.currentNode
              }
                |> deriveVisualEntities
            , Cmd.none
            )


smoothGradient : Model -> Int -> Int -> Float -> Model
smoothGradient model start finish gradient =
    -- This feels like a simple foldl, creating a new list of TrackPoints
    -- which we then splice into the model.
    -- It's a fold because we must keep track of the current elevation
    -- which will increase with each segment.
    let
        segments =
            model.roads |> List.take (finish - 1) |> List.drop start

        startNode =
            Array.get start model.nodeArray

        undoMessage =
            "gradient smoothing from "
                ++ String.fromInt start
                ++ " to "
                ++ String.fromInt finish
                ++ "."
    in
    case startNode of
        Just n ->
            let
                ( _, adjustedTrackPoints ) =
                    List.foldl
                        adjustTrackPoint
                        ( n.trackPoint.ele, [] )
                        segments

                adjustTrackPoint road ( startEle, newTPs ) =
                    let
                        increase =
                            road.length * gradient / 100.0

                        oldTP =
                            road.endsAt.trackPoint
                    in
                    ( startEle + increase
                    , { oldTP
                        | ele = startEle + increase
                      }
                        :: newTPs
                    )
            in
            { model
                | trackPoints =
                    List.take (start + 1) model.trackPoints
                        ++ List.reverse adjustedTrackPoints
                        ++ List.drop finish model.trackPoints
                , undoStack =
                    { label = undoMessage
                    , trackPoints = model.trackPoints
                    }
                        :: model.undoStack
            }

        Nothing ->
            -- shouldn't happen
            model


outputGPX : Model -> Cmd Msg
outputGPX model =
    let
        gpxString =
            writeGPX model.trackName model.trackPoints

        iso8601 =
            Iso8601.fromTime model.time

        outputFilename =
            case model.filename of
                Just fn ->
                    fn ++ "_" ++ iso8601

                Nothing ->
                    iso8601
    in
    Download.string outputFilename "text/gpx" gpxString


deleteZeroLengthSegments : Model -> Model
deleteZeroLengthSegments model =
    -- We have a list of them and their indices.
    -- We remove the troublesome track points and rebuild from there.
    let
        keepNonZero tp =
            not <| List.member tp.idx indexes

        indexes =
            List.map
                (\road -> road.index)
                model.zeroLengths

        reindex points =
            List.map2
                (\p i -> { p | idx = i })
                points
                (List.range 0 (List.length points))
    in
    { model
        | trackPoints = reindex <| List.filter keepNonZero model.trackPoints
        , hasBeenChanged = True
    }


incrementMaybeModulo modulo mx =
    Maybe.map (\x -> modBy modulo (x + 1)) mx


decrementMaybeModulo modulo mx =
    Maybe.map (\x -> modBy modulo (x - 1)) mx


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


parseGPXintoModel : String -> Model -> Model
parseGPXintoModel content model =
    let
        tps =
            parseTrackPoints content
    in
    { model
        | gpx = Just content
        , trackName = parseTrackName content
        , trackPoints = tps
        , hasBeenChanged = False
        , viewingMode =
            if model.trackPoints == [] then
                InputErrorView

            else
                model.viewingMode
    }


deriveNodesAndRoads : Model -> Model
deriveNodesAndRoads model =
    let
        tps =
            model.trackPoints

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

        roadSegments =
            List.map3 roadSegment
                drawingNodes
                (Maybe.withDefault [] <| tail drawingNodes)
                (List.range 0 (List.length drawingNodes))

        roadSegment node1 node2 index =
            let
                zDifference =
                    node2.trackPoint.ele - node1.trackPoint.ele

                earthDistance =
                    -- Great circle distance (!) ignoring elevation difference
                    Spherical.range ( degrees node2.trackPoint.lat, degrees node2.trackPoint.lon )
                        ( degrees node1.trackPoint.lat, degrees node1.trackPoint.lon )
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
        | largestDimension = scalingFactor
        , nodes = drawingNodes
        , roads = roadSegments
        , metresToClipSpace = metresToClipSpace
        , seaLevelInClipSpace = elevationToClipSpace 0.0
        , summary = Just summarise
        , nodeArray = Array.fromList drawingNodes
        , roadArray = Array.fromList roadSegments
    }


resetViewSettings : Model -> Model
resetViewSettings model =
    { model
        | zoomLevelOverview = 1.0
        , zoomLevelFirstPerson = 1.0
        , zoomLevelThirdPerson = 1.0
        , azimuth = Angle.degrees 0.0
        , elevation = Angle.degrees 30.0
        , currentNode = Just 0
        , markedNode = Nothing
    }


deriveProblems : Model -> Model
deriveProblems model =
    let
        suddenGradientChanges =
            List.filterMap identity <|
                -- Filters out Nothings (nice)
                List.map2 compareGradients
                    model.roads
                    (Maybe.withDefault [] <| tail model.roads)

        suddenBearingChanges =
            List.filterMap identity <|
                -- Filters out Nothings (nice)
                List.map2 compareBearings
                    model.roads
                    (Maybe.withDefault [] <| tail model.roads)

        zeroLengths =
            List.filterMap
                (\road ->
                    if road.length == 0.0 then
                        Just road

                    else
                        Nothing
                )
                model.roads

        compareGradients : DrawingRoad -> DrawingRoad -> Maybe AbruptChange
        compareGradients seg1 seg2 =
            -- This list should not include zero length segments; they are separate.
            if
                seg1.length
                    > 0.0
                    && seg2.length
                    > 0.0
                    && abs (seg1.gradient - seg2.gradient)
                    > model.gradientChangeThreshold
            then
                Just
                    { node = seg1.endsAt
                    , before = seg1
                    , after = seg2
                    }

            else
                Nothing

        compareBearings : DrawingRoad -> DrawingRoad -> Maybe AbruptChange
        compareBearings seg1 seg2 =
            -- This list should not include zero length segments; they are separate.
            let
                diff =
                    abs <| seg1.bearing - seg2.bearing

                includedAngle =
                    if diff > pi then
                        pi + pi - diff

                    else
                        diff
            in
            if
                seg1.length
                    > 0.0
                    && seg2.length
                    > 0.0
                    && toDegrees includedAngle
                    > toFloat model.bearingChangeThreshold
            then
                Just
                    { node = seg1.endsAt
                    , before = seg1
                    , after = seg2
                    }

            else
                Nothing
    in
    { model
        | abruptGradientChanges = suddenGradientChanges
        , abruptBearingChanges = suddenBearingChanges
        , zeroLengths = zeroLengths
        , smoothingEndIndex = Nothing
    }


deriveVisualEntities : Model -> Model
deriveVisualEntities model =
    let
        metresToClipSpace =
            model.metresToClipSpace

        seaLevelInClipSpace =
            model.seaLevelInClipSpace

        seaLevel =
            Scene3d.quad (Material.color Color.darkGreen)
                (Point3d.meters -1.2 -1.2 seaLevelInClipSpace)
                (Point3d.meters 1.2 -1.2 seaLevelInClipSpace)
                (Point3d.meters 1.2 1.2 seaLevelInClipSpace)
                (Point3d.meters -1.2 1.2 seaLevelInClipSpace)

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        pointEntities =
            (if model.displayOptions.roadPillars then
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
                    model.nodes

             else
                []
            )
                ++ (if model.displayOptions.roadCones then
                        List.map
                            (\node ->
                                cone (Material.color Color.black) <|
                                    Cone3d.startingAt
                                        (Point3d.meters node.x node.y (node.z - 1.0 * metresToClipSpace))
                                        positiveZ
                                        { radius = meters <| 1.0 * metresToClipSpace
                                        , length = meters <| 1.0 * metresToClipSpace
                                        }
                            )
                            model.nodes

                    else
                        []
                   )

        roadEntities =
            List.concat <|
                List.map roadEntity model.roads

        currentPositionDisc =
            case ( model.currentNode, model.viewingMode ) of
                ( Just idx, ThirdPersonView ) ->
                    case Array.get idx model.roadArray of
                        Just segment ->
                            [ cylinder (Material.color Color.lightOrange) <|
                                Cylinder3d.startingAt
                                    (Point3d.meters
                                        segment.startsAt.x
                                        segment.startsAt.y
                                        segment.startsAt.z
                                    )
                                    (segmentDirection segment)
                                    { radius = meters <| 10.0 * metresToClipSpace
                                    , length = meters <| 1.0 * metresToClipSpace
                                    }
                            ]

                        _ ->
                            []

                _ ->
                    []

        markedNode =
            case ( model.markedNode, model.viewingMode ) of
                ( Just idx, ThirdPersonView ) ->
                    case Array.get idx model.roadArray of
                        Just segment ->
                            [ cone (Material.color Color.purple) <|
                                Cone3d.startingAt
                                    (Point3d.meters
                                        segment.startsAt.x
                                        segment.startsAt.y
                                        segment.startsAt.z
                                    )
                                    (segmentDirection segment)
                                    { radius = meters <| 6.0 * metresToClipSpace
                                    , length = meters <| 3.0 * metresToClipSpace
                                    }
                            ]

                        _ ->
                            []

                _ ->
                    []

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
            (if model.displayOptions.roadTrack then
                [ --surface
                  Scene3d.quad (Material.color Color.grey)
                    (Point3d.meters (segment.startsAt.x + kerbX)
                        (segment.startsAt.y - kerbY)
                        segment.startsAt.z
                    )
                    (Point3d.meters (segment.endsAt.x + kerbX)
                        (segment.endsAt.y - kerbY)
                        segment.endsAt.z
                    )
                    (Point3d.meters (segment.endsAt.x - kerbX)
                        (segment.endsAt.y + kerbY)
                        segment.endsAt.z
                    )
                    (Point3d.meters (segment.startsAt.x - kerbX)
                        (segment.startsAt.y + kerbY)
                        segment.startsAt.z
                    )

                -- kerb walls
                , Scene3d.quad (Material.color Color.darkGrey)
                    (Point3d.meters (segment.startsAt.x + kerbX)
                        (segment.startsAt.y - kerbY)
                        segment.startsAt.z
                    )
                    (Point3d.meters (segment.endsAt.x + kerbX)
                        (segment.endsAt.y - kerbY)
                        segment.endsAt.z
                    )
                    (Point3d.meters (segment.endsAt.x + kerbX)
                        (segment.endsAt.y - kerbY)
                        (segment.endsAt.z + edgeHeight)
                    )
                    (Point3d.meters (segment.startsAt.x + kerbX)
                        (segment.startsAt.y - kerbY)
                        (segment.startsAt.z + edgeHeight)
                    )
                , Scene3d.quad (Material.color Color.darkGrey)
                    (Point3d.meters (segment.startsAt.x - kerbX)
                        (segment.startsAt.y + kerbY)
                        segment.startsAt.z
                    )
                    (Point3d.meters (segment.endsAt.x - kerbX)
                        (segment.endsAt.y + kerbY)
                        segment.endsAt.z
                    )
                    (Point3d.meters (segment.endsAt.x - kerbX)
                        (segment.endsAt.y + kerbY)
                        (segment.endsAt.z + edgeHeight)
                    )
                    (Point3d.meters (segment.startsAt.x - kerbX)
                        (segment.startsAt.y + kerbY)
                        (segment.startsAt.z + edgeHeight)
                    )
                ]

             else
                []
            )
                ++ (if model.displayOptions.gradientFill then
                        [ -- Drop coloured gradient to the ground
                          Scene3d.quad (Material.color <| gradientColour segment.gradient)
                            (Point3d.meters segment.startsAt.x segment.startsAt.y segment.startsAt.z)
                            (Point3d.meters segment.endsAt.x segment.endsAt.y segment.endsAt.z)
                            (Point3d.meters segment.endsAt.x segment.endsAt.y seaLevelInClipSpace)
                            (Point3d.meters segment.startsAt.x segment.startsAt.y seaLevelInClipSpace)
                        ]

                    else
                        []
                   )

        segmentDirection segment =
            let
                maybe =
                    Direction3d.from
                        (Point3d.meters
                            segment.startsAt.x
                            segment.startsAt.y
                            segment.startsAt.z
                        )
                        (Point3d.meters
                            segment.endsAt.x
                            segment.endsAt.y
                            segment.endsAt.z
                        )
            in
            case maybe of
                Just dir ->
                    dir

                Nothing ->
                    positiveZ

        roadToGeometry : DrawingRoad -> G.Road
        roadToGeometry r =
            -- We'll worry about lat and lon afterward.
            -- For now, just try to show a disc in clipspace.
            { startAt = { x = r.startsAt.x, y = r.startsAt.y }
            , endsAt = { x = r.endsAt.x, y = r.endsAt.y }
            }

        tallCylinder x y r =
            cylinder (Material.color Color.white) <|
                Cylinder3d.startingAt
                    (Point3d.meters
                        x
                        y
                        seaLevelInClipSpace
                    )
                    positiveZ
                    { radius = meters <| r
                    , length = meters <| 100.0 * metresToClipSpace
                    }

        bendIncircle =
            -- Right. Let's see if we can fit a circular arc co-tangential
            -- with both end-points. Or disallow if not possible.
            case ( model.thirdPersonSubmode, model.currentNode, model.markedNode ) of
                ( ShowBendFixes, Just current, Just marked ) ->
                    let
                        start =
                            min current marked

                        finis =
                            max current marked
                    in
                    if finis >= start + 2 then
                        let
                            r1 =
                                Array.get start model.roadArray

                            r2 =
                                Array.get (finis - 1) model.roadArray
                        in
                        case ( r1, r2 ) of
                            ( Just road1, Just road2 ) ->
                                let
                                    maybeCircle : Maybe G.Circle
                                    maybeCircle =
                                        G.findIncircleFromTwoRoads
                                            (roadToGeometry road1)
                                            (roadToGeometry road2)
                                in
                                case maybeCircle of
                                    Just circle ->
                                        let
                                            x =
                                                circle.centre.x

                                            y =
                                                circle.centre.y

                                            z =
                                                road1.endsAt.z

                                            r =
                                                circle.radius
                                        in
                                        [ tallCylinder x y r
                                        ]

                                    _ ->
                                        []

                            _ ->
                                []

                    else
                        []

                _ ->
                    []
    in
    { model
        | entities =
            seaLevel
                :: pointEntities
                ++ roadEntities
                ++ currentPositionDisc
                ++ markedNode
                ++ bendIncircle
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

        makeTrackPoint mayLat mayLon mayEle idx =
            case ( mayLat, mayLon, mayEle ) of
                ( Just a, Just b, Just c ) ->
                    Just
                        { lat = a
                        , lon = b
                        , ele = c
                        , idx = idx
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
    List.map4
        makeTrackPoint
        latitudes
        longitudes
        elevations
        (List.range 0 (List.length latitudes))
        |> List.filterMap identity


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


showDecimal x =
    let
        locale =
            { usLocale | decimals = Exact 2 }
    in
    format locale x


viewGenericNew : Model -> Browser.Document Msg
viewGenericNew model =
    { title = "GPX viewer"
    , body =
        [ layout
            [ width fill
            , padding 20
            , spacing 20
            ]
          <|
            column
                [ spacing 10 ]
                [ loadButton
                , displayName model.trackName
                , case model.filename of
                    Just name ->
                        text <| "Filename: " ++ name

                    Nothing ->
                        none
                , row []
                    [ viewModeChoices model
                    ]
                , row []
                    [ view3D model
                    ]
                , el []
                    (saveButtonIfChanged model)
                ]
        ]
    }


loadButton =
    button
        prettyButtonStyles
        { onPress = Just GpxRequested
        , label = text "Load GPX from your computer"
        }


saveButtonIfChanged : Model -> Element Msg
saveButtonIfChanged model =
    case model.undoStack of
        _ :: _ ->
            button
                prettyButtonStyles
                { onPress = Just OutputGPX
                , label = text "Save as GPX file to your computer"
                }

        _ ->
            none


viewModeChoices model =
    Input.radioRow
        [ Border.rounded 6
        , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
        ]
        { onChange = ChooseViewMode
        , selected = Just model.viewingMode
        , label =
            Input.labelHidden "Choose view"
        , options =
            [ Input.optionWith OverviewView <| radioButton First "Overview"
            , Input.optionWith FirstPersonView <| radioButton Mid "First person"
            , Input.optionWith ThirdPersonView <| radioButton Mid "Third person"
            , Input.optionWith ProblemsView <| radioButton Mid "Problems"
            , Input.optionWith OptionsView <| radioButton Last "Options"
            ]
        }


view3D model =
    case model.viewingMode of
        OverviewView ->
            viewPointCloud model

        FirstPersonView ->
            viewRollerCoasterTrackAndControls model

        ThirdPersonView ->
            viewThirdPerson model

        OptionsView ->
            viewOptions model

        ProblemsView ->
            viewAllProblems model

        InputErrorView ->
            viewInputError model


viewInputError : Model -> Element Msg
viewInputError model =
    if model.trackPoints == [] then
        column [ spacing 20 ]
            [ text "I was looking for things like 'lat', 'lon' and 'ele' but didn't find them."
            , case model.gpx of
                Just content ->
                    column []
                        [ text "This is what I found instead."
                        , text <| content
                        ]

                Nothing ->
                    text "<Nothing to see here>"
            ]

    else
        text "That was lovely."


viewAllProblems : Model -> Element Msg
viewAllProblems model =
    column [ spacing 10, padding 10, centerX ]
        [ problemTypeSelectButtons model
        , case model.selectedProblemType of
            ZeroLengthSegment ->
                viewZeroLengthSegments model

            AbruptGradientChanges ->
                viewGradientChanges model

            SharpBends ->
                viewBearingChanges model
        ]


problemTypeSelectButtons model =
    Input.radioRow
        [ Border.rounded 6
        , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
        ]
        { onChange = SelectProblemType
        , selected = Just model.selectedProblemType
        , label =
            Input.labelHidden "Choose problem type"
        , options =
            [ Input.optionWith ZeroLengthSegment <| radioButton First "Zero length"
            , Input.optionWith AbruptGradientChanges <| radioButton Mid "Gradient change"
            , Input.optionWith SharpBends <| radioButton Last "Sharp bend"
            ]
        }


averageGradient : Model -> Int -> Int -> Maybe Float
averageGradient model s f =
    let
        segments =
            model.roads |> List.take f |> List.drop s
    in
    if s < f then
        case ( Array.get s model.nodeArray, Array.get f model.nodeArray ) of
            ( Just startNode, Just endNode ) ->
                let
                    startElevation =
                        startNode.trackPoint.ele

                    endElevation =
                        endNode.trackPoint.ele

                    overallLength =
                        List.sum <| List.map .length segments
                in
                Just <| (endElevation - startElevation) / overallLength * 100.0

            _ ->
                Nothing

    else
        Nothing


viewGradientChanges : Model -> Element Msg
viewGradientChanges model =
    let
        idx change =
            change.node.trackPoint.idx

        linkButton change =
            button prettyButtonStyles
                { onPress = Just (UserMovedNodeSlider (idx change))
                , label = text <| String.fromInt (idx change)
                }
    in
    column [ spacing 10, padding 20 ]
        [ gradientChangeThresholdSlider model
        , wrappedRow [ width <| px 300 ] <|
            List.map linkButton model.abruptGradientChanges
        ]


viewBearingChanges : Model -> Element Msg
viewBearingChanges model =
    let
        idx change =
            change.node.trackPoint.idx

        linkButton change =
            button prettyButtonStyles
                { onPress = Just (UserMovedNodeSlider (idx change))
                , label = text <| String.fromInt (idx change)
                }
    in
    column [ spacing 10, padding 20 ]
        [ gradientChangeThresholdSlider model
        , wrappedRow [ width <| px 300 ] <|
            List.map linkButton model.abruptBearingChanges
        ]


buttonHighlightCurrent : Int -> Model -> List (Attribute msg)
buttonHighlightCurrent index model =
    if Just index == model.currentNode then
        [ Background.color <| rgb255 114 159 207, alignRight ]

    else
        [ Background.color <| rgb255 0xFF 0xFF 0xFF, alignRight ]


buttonSmoothingEnd : Int -> Model -> List (Attribute msg)
buttonSmoothingEnd index model =
    if Just index == model.smoothingEndIndex then
        [ Background.color <| rgb255 114 159 207, alignRight ]

    else
        [ Background.color <| rgb255 0xFF 0xFF 0xFF, alignRight ]


viewZeroLengthSegments : Model -> Element Msg
viewZeroLengthSegments model =
    el [ spacing 10, padding 20 ] <|
        if List.length model.zeroLengths > 0 then
            column [ spacing 10 ]
                [ table [ width fill, centerX, spacing 10 ]
                    { data = model.zeroLengths
                    , columns =
                        [ { header = text "Track point\n(Click to pick)"
                          , width = fill
                          , view =
                                \z ->
                                    button (buttonHighlightCurrent z.index model)
                                        { onPress = Just (UserMovedNodeSlider z.index)
                                        , label = text <| String.fromInt z.index
                                        }
                          }
                        ]
                    }
                , button
                    prettyButtonStyles
                    { onPress = Just DeleteZeroLengthSegments
                    , label = text "Delete these segments"
                    }
                ]

        else
            text "There are no zero-length segments to see here."


checkboxIcon : Bool -> Element msg
checkboxIcon isChecked =
    el
        [ width <| px 32
        , height <| px 32
        , centerY
        , padding 4
        , Border.rounded 6
        , Border.width 2
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    rgb255 114 159 207

                else
                    rgb255 0xFF 0xFF 0xFF
            ]
        <|
            none


viewOptions : Model -> Element Msg
viewOptions model =
    row [ centerX ]
        [ column [ padding 50, alignTop, spacing 10 ]
            [ paragraph
                [ padding 20
                , Font.size 24
                ]
              <|
                [ text "Select view elements" ]
            , Input.checkbox [ Font.size 18 ]
                { onChange = ToggleGradient
                , icon = checkboxIcon
                , checked = model.displayOptions.gradientFill
                , label = Input.labelRight [] (text "Gradient colours")
                }
            , Input.checkbox [ Font.size 18 ]
                { onChange = ToggleRoad
                , icon = checkboxIcon
                , checked = model.displayOptions.roadTrack
                , label = Input.labelRight [] (text "Road surface")
                }
            , Input.checkbox [ Font.size 18 ]
                { onChange = TogglePillars
                , icon = checkboxIcon
                , checked = model.displayOptions.roadPillars
                , label = Input.labelRight [] (text "Road support pillars")
                }
            , Input.checkbox [ Font.size 18 ]
                { onChange = ToggleCones
                , icon = checkboxIcon
                , checked = model.displayOptions.roadCones
                , label = Input.labelRight [] (text "Trackpoint cones")
                }
            , gradientChangeThresholdSlider model
            ]
        , paragraph [ width <| px 600 ] <| [ html <| Markdown.toHtml [] aboutText ]
        ]


commonShortHorizontalSliderStyles =
    [ height <| px 30
    , width <| px 200
    , centerY
    , behindContent <|
        -- Slider track
        el
            [ width <| px 200
            , height <| px 30
            , centerY
            , centerX
            , Background.color <| rgb255 114 159 207
            , Border.rounded 6
            ]
            Element.none
    ]


gradientChangeThresholdSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetGradientChangeThreshold
        , label =
            Input.labelBelow [] <|
                text <|
                    "Gradient change threshold = "
                        ++ showDecimal model.gradientChangeThreshold
        , min = 5.0
        , max = 20.0
        , step = Nothing
        , value = model.gradientChangeThreshold
        , thumb = Input.defaultThumb
        }


bearingChangeThresholdSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetBearingChangeThreshold
        , label =
            Input.labelBelow [] <|
                text <|
                    "Direction change threshold = "
                        ++ String.fromInt model.bearingChangeThreshold
        , min = 30.0
        , max = 120.0
        , step = Just 1.0
        , value = toFloat model.bearingChangeThreshold
        , thumb = Input.defaultThumb
        }


aboutText =
    """Thank you for trying this GPX viewer. It is freely provided without warranty.

**Overview** shows a route overview and summary statistics. Zoom is (currently) fixed on the centre of the area and there is no pan capability.

**First person** positions the viewpoint above a track _road segment_, sighted along the track. The zoom control will move your viewpoint back and forth a bit. The bottom slider and arrows move between track segments. Information about the current segment is shown.

**Third person** focuses on track _points_ and lets you fly around the current point.  An orange disc will indicate the position on the track. The slider and arrows move to other track points. Information about the current track point is shown. **NEW** You can apply fixes in this view using the "Fixes" section on the right hand side.

**Problems** details issues with the GPX that you may want to "fix".

**Options** (this page) lets you turn off some elements. You may like this depending on your motivation; it may help to turn some things off with a complex track. _Gradient change threshold_ will cause markers to hover over any track points where the gradient change exceed this value.

Simply click the blue button at the page top to choose a file.

> _Peter Ward, 2020_
"""


viewPointCloud model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0.0 0.0 0.0
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters <| distanceFromZoom model model.zoomLevelOverview
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        showSummary =
            case model.summary of
                Just summary ->
                    row [ padding 20 ]
                        [ column [ spacing 10 ]
                            [ text "Highest point "
                            , text "Lowest point "
                            , text "Track length "
                            , text "Climbing distance "
                            , text "Elevation gain "
                            , text "Descending distance "
                            , text "Elevation loss "
                            ]
                        , column [ spacing 10 ]
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
        [ zoomSlider model.zoomLevelOverview ZoomLevelOverview
        , el
            withMouseCapture
          <|
            html <|
                Scene3d.sunny
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.backgroundColor Color.lightBlue
                    , clipDepth = Length.meters (1.0 * model.metresToClipSpace)
                    , entities = model.entities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        , showSummary
        ]


toDegrees rads =
    rads * 180.0 / pi


positionControls model =
    row
        [ spacing 5
        , padding 5
        , Border.width 1
        , centerX
        , centerY
        ]
        [ positionSlider model
        , button
            prettyButtonStyles
            { onPress = Just PositionBackOne
            , label = text "◀︎"
            }
        , button
            prettyButtonStyles
            { onPress = Just PositionForwardOne
            , label = text "►︎"
            }
        ]


positionSlider model =
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
        { onChange = UserMovedNodeSlider << round
        , label =
            Input.labelHidden "Drag slider or use arrow buttons"
        , min = 1.0
        , max = toFloat <| List.length model.roads - 1
        , step = Just 1
        , value = toFloat <| Maybe.withDefault 0 model.currentNode
        , thumb = Input.defaultThumb
        }


viewRollerCoasterTrackAndControls model =
    let
        getRoad : Maybe DrawingRoad
        getRoad =
            -- N.B. will fail on last node.
            case model.currentNode of
                Just n ->
                    Array.get n model.roadArray

                _ ->
                    Nothing
    in
    case getRoad of
        Nothing ->
            none

        Just road ->
            row []
                [ zoomSlider model.zoomLevelFirstPerson ZoomLevelFirstPerson
                , column []
                    [ viewRoadSegment model road
                    , positionControls model
                    ]
                , row [ padding 20 ]
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
                        [ text <| String.fromInt <| Maybe.withDefault 1 model.currentNode
                        , text <| showDecimal road.startsAt.trackPoint.lat
                        , text <| showDecimal road.startsAt.trackPoint.lon
                        , text <| showDecimal road.startsAt.trackPoint.ele
                        , text <| showDecimal road.endsAt.trackPoint.lat
                        , text <| showDecimal road.endsAt.trackPoint.lon
                        , text <| showDecimal road.endsAt.trackPoint.ele
                        , text <| showDecimal road.length
                        , text <| showDecimal road.gradient
                        , text <| bearingToDisplayDegrees road.bearing
                        ]
                    ]
                ]


bearingToDisplayDegrees x =
    showDecimal <|
        toDegrees <|
            if x < 0 then
                pi + pi + x

            else
                x


withMouseCapture =
    [ htmlAttribute <| Pointer.onDown (\event -> ImageGrab event.pointer.offsetPos)
    , htmlAttribute <| Pointer.onMove (\event -> ImageRotate event.pointer.offsetPos)
    , htmlAttribute <| Pointer.onUp (\event -> ImageRelease event.pointer.offsetPos)
    , htmlAttribute <| style "touch-action" "none"
    , width fill
    , pointer
    ]


viewRoadSegment model road =
    let
        eyeHeight =
            -- Helps to be higher up.
            2.0 * model.metresToClipSpace

        cameraSetback =
            0.0 * model.metresToClipSpace

        cameraViewpoint someTarmac =
            Viewpoint3d.lookAt
                { eyePoint =
                    Point3d.meters
                        (someTarmac.startsAt.x - cameraSetback * sin road.bearing)
                        (someTarmac.startsAt.y - cameraSetback * cos road.bearing)
                        (someTarmac.startsAt.z + eyeHeight)
                , focalPoint =
                    Point3d.meters
                        someTarmac.endsAt.x
                        someTarmac.endsAt.y
                        (someTarmac.endsAt.z + eyeHeight)
                , upDirection = Direction3d.positiveZ
                }

        camera someTarmac =
            Camera3d.perspective
                { viewpoint = cameraViewpoint someTarmac
                , verticalFieldOfView = Angle.degrees <| 120.0 / model.zoomLevelFirstPerson
                }
    in
    el [] <|
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


zoomSlider value msg =
    Input.slider
        [ height <| px 400
        , width <| px 80
        , alignTop
        , behindContent <|
            -- Slider track
            el
                [ width <| px 30
                , height <| px 400
                , alignTop
                , centerX
                , Background.color <| rgb255 114 159 207
                , Border.rounded 6
                ]
                Element.none
        ]
        { onChange = msg
        , label =
            Input.labelHidden "Zoom"
        , min = 1.0
        , max = 4.0
        , step = Nothing
        , value = value
        , thumb = Input.defaultThumb
        }


viewThirdPerson : Model -> Element Msg
viewThirdPerson model =
    -- Let's the user spin around and zoom in on any road point.
    let
        getNodeNum =
            case model.currentNode of
                Just n ->
                    n

                Nothing ->
                    0

        getNode =
            case model.currentNode of
                Just n ->
                    Array.get n model.nodeArray

                Nothing ->
                    Nothing
    in
    case getNode of
        Nothing ->
            none

        Just node ->
            row [ alignTop ]
                [ column
                    [ alignTop
                    ]
                    [ viewCurrentNode model node
                    , positionControls model
                    ]
                , viewThirdPersonSubpane model
                ]


viewThirdPersonSubpane : Model -> Element Msg
viewThirdPersonSubpane model =
    column [ alignTop, padding 20, spacing 10 ]
        [ Input.radioRow
            [ Border.rounded 6
            , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
            ]
            { onChange = SetThirdPersonSubmode
            , selected = Just model.thirdPersonSubmode
            , label =
                Input.labelHidden "Choose mode"
            , options =
                [ Input.optionWith ShowData <| radioButton First "Location\ndata"
                , Input.optionWith ShowGradientFixes <| radioButton Mid "Gradient\nsmoother"
                , Input.optionWith ShowBendFixes <| radioButton Last "Bend\nsmoother"
                ]
            }
        , case model.thirdPersonSubmode of
            ShowData ->
                viewSummaryStats model

            ShowGradientFixes ->
                viewGradientFixerPane model

            ShowBendFixes ->
                viewBendFixerPane model
        ]


viewBendFixerPane : Model -> Element Msg
viewBendFixerPane model =
    column []
        [ text "Sorry, we can't fix the bends yet."
        , markerButton model
        , viewBearingChanges model
        ]


markerButton model =
    let
        makeButton label =
            button
                prettyButtonStyles
                { onPress = Just ToggleMarker
                , label =
                    text <| label
                }
    in
    row [ spacing 5, padding 5, Border.width 1 ] <|
        case model.markedNode of
            Just _ ->
                [ button
                    prettyButtonStyles
                    { onPress = Just MarkerBackOne
                    , label = text "◀︎"
                    }
                , makeButton "Clear marker"
                , button
                    prettyButtonStyles
                    { onPress = Just MarkerForwardOne
                    , label = text "►︎"
                    }
                ]

            Nothing ->
                [ makeButton "Drop marker" ]


undoButton model =
    button
        prettyButtonStyles
        { onPress =
            case model.undoStack of
                [] ->
                    Nothing

                _ ->
                    Just Undo
        , label =
            case model.undoStack of
                u :: _ ->
                    text <| "Undo " ++ u.label

                _ ->
                    text "Nothing to undo"
        }


viewGradientFixerPane : Model -> Element Msg
viewGradientFixerPane model =
    let
        gradientSmoothButton =
            case ( model.currentNode, model.markedNode ) of
                ( Just c, Just m ) ->
                    let
                        start =
                            min c m

                        finish =
                            max c m

                        avg =
                            averageGradient model start finish
                    in
                    case avg of
                        Just gradient ->
                            button
                                prettyButtonStyles
                                { onPress = Just <| SmoothGradient start finish gradient
                                , label =
                                    text <|
                                        "Smooth between markers\nAverage gradient "
                                            ++ showDecimal gradient
                                }

                        _ ->
                            none

                _ ->
                    none
    in
    column [ spacing 10 ]
        [ markerButton model
        , gradientSmoothButton
        , undoButton model
        , saveButtonIfChanged model
        , viewGradientChanges model
        ]


lookupRoad : Model -> Maybe Int -> Maybe DrawingRoad
lookupRoad model idx =
    -- Have I not written this already?
    case idx of
        Just i ->
            Array.get i model.roadArray

        _ ->
            Nothing


viewSummaryStats : Model -> Element Msg
viewSummaryStats model =
    let
        getNodeNum =
            case model.currentNode of
                Just n ->
                    n

                Nothing ->
                    0
    in
    case Array.get getNodeNum model.nodeArray of
        Just node ->
            row [ padding 20 ]
                [ column [ spacing 10 ]
                    [ text "Index "
                    , text "Latitude "
                    , text "Longitude "
                    , text "Elevation "
                    ]
                , column [ spacing 10 ]
                    [ text <| String.fromInt <| getNodeNum
                    , text <| showDecimal node.trackPoint.lat
                    , text <| showDecimal node.trackPoint.lon
                    , text <| showDecimal node.trackPoint.ele
                    ]
                ]

        Nothing ->
            none


distanceFromZoom model zoomLevel =
    1.0 * model.metresToClipSpace * 10 ^ (5.0 - zoomLevel)


viewCurrentNode : Model -> DrawingNode -> Element Msg
viewCurrentNode model node =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint =
                            Point3d.meters node.x node.y node.z
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters <| distanceFromZoom model model.zoomLevelThirdPerson
                        }
                , verticalFieldOfView = Angle.degrees <| 20 * model.zoomLevelThirdPerson
                }
    in
    row []
        [ zoomSlider model.zoomLevelThirdPerson ZoomLevelThirdPerson
        , el
            withMouseCapture
          <|
            html <|
                Scene3d.sunny
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.backgroundColor Color.lightBlue
                    , clipDepth = Length.meters (1.0 * model.metresToClipSpace)
                    , entities = model.entities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        ]


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


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
