module Main exposing (main)

import Angle exposing (Angle, inDegrees, normalize)
import Array exposing (Array)
import BendSmoother exposing (SmoothedBend, bendIncircle)
import Browser
import Camera3d
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (color)
import Element.Font as Font
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Iso8601
import Length exposing (meters)
import List exposing (drop, tail, take)
import Markdown exposing (defaultOptions)
import Msg exposing (..)
import NodesAndRoads exposing (DrawingNode, DrawingRoad, MyCoord)
import Pixels exposing (Pixels)
import Point3d
import Scene3d exposing (Entity, cone, cylinder)
import Scene3d.Material as Material
import Spherical
import Task
import Time
import TrackPoint exposing (..)
import Utils exposing (..)
import ViewElements exposing (loadButton, prettyButtonStyles)
import ViewTypes exposing (..)
import Viewpoint3d
import VisualEntities exposing (makeVisualEntities)
import WriteGPX exposing (writeGPX)



--TODO: Linear orthographic ride profile.
--TODO: Break up this module.
--TODO: Try chamfer for horizontal turns.
--TODO: Autofix bends with circular arcs.
--TODO: Improve removal of zero lengths so we don't have to repeat ourselves!
--TODO: Allow drag & zoom in 1st person including flythrough (offset, not angles).
--TODO: Some way to join the start and end of a loop (although MR does this).
--TODO: Remove duplicate helpers.


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = viewGenericNew
        , update = update
        , subscriptions = subscriptions
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


type alias Model =
    { gpx : Maybe String
    , filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , gpxUrl : String
    , trackPoints : List TrackPoint
    , scaling : Maybe ScalingInfo
    , nodes : List DrawingNode
    , roads : List DrawingRoad
    , trackName : Maybe String
    , azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Maybe Point -- Capture mouse down position (when clicking on the 3D control)
    , entities : List (Entity MyCoord) -- our 3D world
    , profileEntities : List (Entity MyCoord) -- an unrolled 3D world for the profile view.
    , httpError : Maybe String
    , currentNode : Maybe Int
    , markedNode : Maybe Int
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
    , hasBeenChanged : Bool
    , smoothingEndIndex : Maybe Int
    , undoStack : List UndoEntry
    , thirdPersonSubmode : ThirdPersonSubmode
    , smoothedBend : Maybe SmoothedBend
    , smoothedNodes : List DrawingNode
    , smoothedRoads : List DrawingRoad
    , maxTurnPerSegment : Float
    , bumpinessFactor : Float -- 0.0 => average gradient, 1 => original gradients
    , flythroughSpeed : Float
    , flythrough : Maybe Flythrough
    }


type alias Flythrough =
    { cameraPosition : ( Float, Float, Float )
    , focusPoint : ( Float, Float, Float )
    , metresFromRouteStart : Float
    , lastUpdated : Time.Posix
    , running : Bool
    , segment : DrawingRoad
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , gpxUrl = ""
      , trackPoints = []
      , scaling = Nothing
      , nodes = []
      , roads = []
      , trackName = Nothing
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = Nothing
      , entities = []
      , profileEntities = []
      , httpError = Nothing
      , currentNode = Nothing
      , markedNode = Nothing
      , viewingMode = AboutView
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
      , hasBeenChanged = False
      , smoothingEndIndex = Nothing
      , undoStack = []
      , thirdPersonSubmode = ShowData
      , smoothedBend = Nothing
      , smoothedNodes = []
      , smoothedRoads = []
      , maxTurnPerSegment = 20.0
      , bumpinessFactor = 0.0
      , flythrough = Nothing
      , flythroughSpeed = 1.0
      }
    , Task.perform AdjustTimeZone Time.here
    )


metresPerDegreeLatitude =
    78846.81


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        options =
            model.displayOptions
    in
    case msg of
        Tick newTime ->
            ( { model | time = newTime } |> advanceFlythrough newTime
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
                |> deriveProblems
                |> deleteZeroLengthSegments
                |> deriveProblems
                |> deriveVisualEntities
                |> resetViewSettings
            , Cmd.none
            )

        UserMovedNodeSlider node ->
            ( { model | currentNode = Just node }
                |> tryBendSmoother
                |> deriveVisualEntities
                |> cancelFlythrough
            , Cmd.none
            )

        SetSmoothingEnd idx ->
            ( { model | smoothingEndIndex = Just idx }
                |> tryBendSmoother
                |> deriveVisualEntities
            , Cmd.none
            )

        PositionForwardOne ->
            ( { model
                | currentNode = incrementMaybeModulo (List.length model.roads - 1) model.currentNode
              }
                |> tryBendSmoother
                |> deriveVisualEntities
                |> cancelFlythrough
            , Cmd.none
            )

        PositionBackOne ->
            ( { model
                | currentNode = decrementMaybeModulo (List.length model.roads - 1) model.currentNode
              }
                |> tryBendSmoother
                |> deriveVisualEntities
                |> cancelFlythrough
            , Cmd.none
            )

        MarkerForwardOne ->
            ( { model
                | markedNode = incrementMaybeModulo (List.length model.roads - 1) model.markedNode
              }
                |> tryBendSmoother
                |> deriveVisualEntities
            , Cmd.none
            )

        MarkerBackOne ->
            ( { model
                | markedNode = decrementMaybeModulo (List.length model.roads - 1) model.markedNode
              }
                |> tryBendSmoother
                |> deriveVisualEntities
            , Cmd.none
            )

        SetMaxTurnPerSegment turn ->
            ( { model
                | maxTurnPerSegment = turn
              }
                |> tryBendSmoother
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

        TogglePillars style ->
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

        SetCurtainStyle style ->
            ( { model
                | displayOptions = { options | curtainStyle = style }
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

        SetFlythroughSpeed speed ->
            ( { model
                | flythroughSpeed = speed
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
                |> tryBendSmoother
                |> deriveVisualEntities
            , Cmd.none
            )

        SetBumpinessFactor factor ->
            ( { model | bumpinessFactor = factor }
            , Cmd.none
            )

        RunFlythrough isOn ->
            ( if isOn then
                startFlythrough model

              else
                pauseFlythrough model
            , Cmd.none
            )

        ResetFlythrough ->
            ( resetFlythrough model
            , Cmd.none
            )

        VerticalNodeSpilt node ->
            ( verticalNodeSplit node model
                |> deriveNodesAndRoads
                |> deriveVisualEntities
                |> deriveProblems
                |> (\m -> { m | currentNode = model.currentNode })
            , Cmd.none
            )


verticalNodeSplit : Int -> Model -> Model
verticalNodeSplit n model =
    -- Replace the current node with two close nodes that each have half the gradient change.
    -- 'Close' being perhaps the lesser of one metre and a third segment length.
    -- Lat and Lon to be linear interpolation.
    let
        undoMessage =
            "Vertical chamfer at " ++ String.fromInt n
    in
    case ( Array.get (n - 1) model.roadArray, Array.get n model.roadArray ) of
        ( Just before, Just after ) ->
            let
                newChange =
                    0.5 * (after.gradient - before.gradient)

                placeInFirstSegment =
                    max (before.length - 1.0) (before.length / 2.0)

                placeInSecondSegment =
                    min 1.0 (after.length / 2.0)

                firstTP =
                    interpolateSegment
                        (placeInFirstSegment / before.length)
                        before.startsAt.trackPoint
                        before.endsAt.trackPoint

                secondTP =
                    interpolateSegment
                        (placeInSecondSegment / after.length)
                        after.startsAt.trackPoint
                        after.endsAt.trackPoint

                precedingTPs =
                    model.trackPoints |> List.take n

                remainingTPs =
                    model.trackPoints |> List.drop (n + 1)

                newTPs =
                    precedingTPs
                        ++ [ firstTP, secondTP ]
                        ++ remainingTPs
            in
            { model
                | trackPoints = reindexTrackpoints newTPs
                , undoStack =
                    { label = undoMessage
                    , trackPoints = model.trackPoints
                    }
                        :: model.undoStack
            }

        _ ->
            model


startFlythrough : Model -> Model
startFlythrough model =
    case model.flythrough of
        Just flying ->
            { model
                | flythrough =
                    Just
                        { flying
                            | running = True
                            , lastUpdated = model.time
                        }
            }

        Nothing ->
            resetFlythrough model |> startFlythrough


cancelFlythrough : Model -> Model
cancelFlythrough model =
    { model | flythrough = Nothing }


pauseFlythrough : Model -> Model
pauseFlythrough model =
    case model.flythrough of
        Just flying ->
            { model
                | flythrough =
                    Just
                        { flying
                            | running = False
                        }
            }

        Nothing ->
            model


advanceFlythrough : Time.Posix -> Model -> Model
advanceFlythrough newTime model =
    let
        findRoadByDistance d roads =
            case roads of
                r1 :: rN ->
                    if d >= r1.startDistance && d < r1.endDistance then
                        Just r1

                    else
                        findRoadByDistance d rN

                [] ->
                    Nothing
    in
    case model.flythrough of
        Nothing ->
            model

        Just flying ->
            if flying.running then
                let
                    groundSpeed =
                        10.0 ^ model.flythroughSpeed

                    newDistance =
                        flying.metresFromRouteStart + tempus * groundSpeed

                    tempus =
                        toFloat (Time.posixToMillis newTime - Time.posixToMillis flying.lastUpdated) / 1000.0

                    currentSegment =
                        findRoadByDistance newDistance model.roads
                in
                case ( currentSegment, model.scaling ) of
                    ( Just seg, Just scale ) ->
                        { model
                            | flythrough =
                                let
                                    segInsetMetres =
                                        newDistance - seg.startDistance

                                    segFraction =
                                        segInsetMetres / seg.length

                                    x =
                                        segFraction * seg.endsAt.x + (1 - segFraction) * seg.startsAt.x

                                    y =
                                        segFraction * seg.endsAt.y + (1 - segFraction) * seg.startsAt.y

                                    z =
                                        segFraction * seg.endsAt.z + (1 - segFraction) * seg.startsAt.z

                                    nextSeg =
                                        Array.get (seg.index + 1) model.roadArray

                                    lookingAt =
                                        case nextSeg of
                                            Just next ->
                                                -- Start looking around the bend, makin flythrough smoother.
                                                let
                                                    nextX =
                                                        (next.startsAt.x + next.endsAt.x) / 2.0

                                                    nextY =
                                                        (next.startsAt.y + next.endsAt.y) / 2.0

                                                    nextZ =
                                                        (next.startsAt.z + next.endsAt.z) / 2.0

                                                    focusX =
                                                        (nextX + seg.endsAt.x) / 2.0

                                                    focusY =
                                                        (nextY + seg.endsAt.y) / 2.0

                                                    focusZ =
                                                        (nextZ + seg.endsAt.z) / 2.0
                                                in
                                                ( focusX, focusY, focusZ )

                                            Nothing ->
                                                ( seg.endsAt.x, seg.endsAt.y, seg.endsAt.z )
                                in
                                Just
                                    { flying
                                        | metresFromRouteStart = newDistance
                                        , lastUpdated = newTime
                                        , segment = seg
                                        , cameraPosition = ( x, y, z )
                                        , focusPoint = lookingAt
                                    }
                        }

                    _ ->
                        { model | flythrough = Just { flying | running = False } }

            else
                model


resetFlythrough : Model -> Model
resetFlythrough model =
    case model.currentNode of
        Just node ->
            case Array.get node model.roadArray of
                Just road ->
                    { model
                        | flythrough =
                            Just
                                { metresFromRouteStart = road.startDistance
                                , running = False
                                , cameraPosition = ( road.startsAt.x, road.startsAt.y, road.startsAt.z )
                                , focusPoint = ( road.endsAt.x, road.endsAt.y, road.endsAt.z )
                                , lastUpdated = model.time
                                , segment = road
                                }
                    }

                Nothing ->
                    -- Why no road? Something amiss.
                    { model | flythrough = Nothing }

        _ ->
            model


tryBendSmoother : Model -> Model
tryBendSmoother model =
    case ( model.scaling, model.currentNode, model.markedNode ) of
        ( Just scale, Just c, Just m ) ->
            let
                ( n1, n2 ) =
                    ( min c m, max c m )

                entrySegment =
                    Array.get n1 model.roadArray

                exitSegment =
                    Array.get (n2 - 1) model.roadArray
            in
            case ( entrySegment, exitSegment ) of
                ( Just road1, Just road2 ) ->
                    let
                        pa =
                            road1.startsAt.trackPoint

                        pb =
                            road1.endsAt.trackPoint

                        pc =
                            road2.startsAt.trackPoint

                        pd =
                            road2.endsAt.trackPoint

                        newTrack =
                            bendIncircle 10.0 pa pb pc pd
                    in
                    case newTrack of
                        Just track ->
                            let
                                newNodes =
                                    deriveNodes scale track.trackPoints

                                newRoads =
                                    deriveRoads newNodes
                            in
                            { model
                                | smoothedBend = newTrack
                                , smoothedNodes = newNodes
                                , smoothedRoads = newRoads
                            }

                        _ ->
                            { model | smoothedBend = Nothing }

                _ ->
                    { model | smoothedBend = Nothing }

        _ ->
            { model | smoothedBend = Nothing }


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
                ++ ", \nbumpiness "
                ++ showDecimal2 model.bumpinessFactor
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
                    -- This would be the elevations along the average, bumpiness == 0.0
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

                bumpyTrackPoints =
                    -- Intermediate between original and smooth
                    List.map2
                        applyBumpiness
                        (List.reverse adjustedTrackPoints)
                        segments

                applyBumpiness newTP oldSeg =
                    let
                        oldTP =
                            oldSeg.endsAt.trackPoint
                    in
                    { oldTP
                        | ele =
                            model.bumpinessFactor
                                * oldTP.ele
                                + (1.0 - model.bumpinessFactor)
                                * newTP.ele
                    }
            in
            { model
                | trackPoints =
                    List.take (start + 1) model.trackPoints
                        ++ bumpyTrackPoints
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
                    let
                        dropAfterLastDot s =
                            s
                                |> String.split "."
                                |> List.reverse
                                |> List.drop 1
                                |> List.reverse
                                |> String.join "."

                        prefix =
                            dropAfterLastDot fn

                        timestamp =
                            dropAfterLastDot iso8601

                        suffix =
                            "gpx"
                    in
                    prefix ++ "_" ++ timestamp ++ "." ++ suffix

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
    in
    { model
        | trackPoints = reindexTrackpoints <| List.filter keepNonZero model.trackPoints
    }


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


deriveNodes : ScalingInfo -> List TrackPoint -> List DrawingNode
deriveNodes scale tps =
    let
        elevationToClipSpace e =
            (e - findCentres.ele) * scale.metresToClipSpace

        findCentres =
            scale.centres

        mins =
            scale.mins

        prepareDrawingNode tp =
            { trackPoint = tp
            , northOffset = (tp.lat - mins.lat) * metresPerDegreeLatitude
            , eastOffset = (tp.lon - mins.lon) * metresPerDegreeLatitude * cos tp.lat
            , vertOffset = tp.ele - mins.ele
            , x = (tp.lon - findCentres.lon) / (0.5 * scale.largestDimension)
            , y = (tp.lat - findCentres.lat) / (0.5 * scale.largestDimension)
            , z = elevationToClipSpace tp.ele
            }
    in
    List.map prepareDrawingNode tps


deriveRoads : List DrawingNode -> List DrawingRoad
deriveRoads drawingNodes =
    let
        roadSegment node1 node2 =
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
            , index = 0
            }

        roadSegments =
            List.map2 roadSegment
                drawingNodes
                (List.drop 1 drawingNodes)

        ( _, _, withAccumulations ) =
            List.foldl
                (\road ( idx, dist, done ) ->
                    ( idx + 1
                    , dist + road.length
                    , { road
                        | startDistance = dist
                        , endDistance = dist + road.length
                        , index = idx
                      }
                        :: done
                    )
                )
                ( 0, 0.0, [] )
                roadSegments
    in
    List.reverse withAccumulations


deriveSummary : List DrawingRoad -> SummaryData
deriveSummary roadSegments =
    let
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
    in
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


deriveNodesAndRoads : Model -> Model
deriveNodesAndRoads model =
    let
        withScaling m =
            { m | scaling = Just <| deriveScalingInfo m.trackPoints }

        withNodes m =
            case m.scaling of
                Just scale ->
                    { m | nodes = deriveNodes scale m.trackPoints }

                Nothing ->
                    m

        withRoads m =
            { m | roads = deriveRoads m.nodes }

        withSummary m =
            { m | summary = Just <| deriveSummary m.roads }

        withArrays m =
            { m
                | nodeArray = Array.fromList m.nodes
                , roadArray = Array.fromList m.roads
            }
    in
    model
        |> withScaling
        |> withNodes
        |> withRoads
        |> withSummary
        |> withArrays


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
        , viewingMode = OverviewView
        , flythrough = Nothing
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
        currentRoad =
            lookupRoad model model.currentNode

        markedRoad =
            lookupRoad model model.markedNode
    in
    case model.scaling of
        Just scale ->
            let
                context =
                    { displayOptions = model.displayOptions
                    , currentNode = currentRoad
                    , markedNode = markedRoad
                    , scaling = scale
                    , viewingMode = model.viewingMode
                    , viewingSubMode = model.thirdPersonSubmode
                    , smoothedBend = model.smoothedRoads
                    }
            in
            { model | entities = makeVisualEntities context model.roadArray }

        Nothing ->
            model


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
                [ row [ centerX, spaceEvenly, spacing 20 ]
                    [ loadButton
                    , case model.filename of
                        Just name ->
                            column []
                                [ displayName model.trackName
                                , text <| "Filename: " ++ name
                                ]

                        Nothing ->
                            none
                    , saveButtonIfChanged model
                    ]
                , row []
                    [ viewModeChoices model
                    ]
                , case model.scaling of
                    Just scale ->
                        row []
                            [ view3D scale model ]

                    Nothing ->
                        viewAboutText
                ]
        ]
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


viewModeChoices : Model -> Element Msg
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

            --, Input.optionWith ProblemsView <| radioButton Mid "Problems"
            , Input.optionWith AboutView <| radioButton Last "About"
            ]
        }


view3D : ScalingInfo -> Model -> Element Msg
view3D scale model =
    case model.viewingMode of
        OverviewView ->
            viewPointCloud scale model

        FirstPersonView ->
            viewFirstPerson scale model

        ThirdPersonView ->
            viewThirdPerson scale model

        AboutView ->
            viewAboutText

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
        [ bearingChangeThresholdSlider model
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
    column [ padding 50, alignTop, spacing 10 ]
        [ paragraph
            [ padding 20
            , Font.size 24
            ]
          <|
            [ text "Select view elements" ]
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
        , Input.radioRow
            [ Border.rounded 6
            , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
            ]
            { onChange = SetCurtainStyle
            , selected = Just model.displayOptions.curtainStyle
            , label =
                Input.labelBelow [ centerX ] <| text "Curtain style"
            , options =
                [ Input.optionWith NoCurtain <| radioButton First "None"
                , Input.optionWith PlainCurtain <| radioButton Mid "Plain"
                , Input.optionWith RainbowCurtain <| radioButton Last "Rainbow"
                ]
            }
        ]


viewAboutText : Element Msg
viewAboutText =
    row [ centerX ]
        [ paragraph [ width <| px 800 ] <| [ html <| Markdown.toHtml [] aboutText ]
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
                        ++ showDecimal2 model.gradientChangeThreshold
        , min = 5.0
        , max = 20.0
        , step = Nothing
        , value = model.gradientChangeThreshold
        , thumb = Input.defaultThumb
        }


bearingChangeThresholdSlider : Model -> Element Msg
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


bendSmoothnessSlider : Model -> Element Msg
bendSmoothnessSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetMaxTurnPerSegment
        , label =
            Input.labelBelow [] <|
                text <|
                    "Maximum turn per segment = "
                        ++ String.fromFloat model.maxTurnPerSegment
        , min = 10.0
        , max = 30.0
        , step = Just 1.0
        , value = model.maxTurnPerSegment
        , thumb = Input.defaultThumb
        }


aboutText =
    """Thank you for trying this GPX viewer. It is freely provided without warranty.

> _This text updated 2020-11-21_

**Overview** shows a route overview and summary statistics. Zoom is (currently) fixed on the centre of the area and there is no pan capability. You can change the presentation style using the options on the right.

**First person** positions the viewpoint above a track _road segment_, sighted along the track. The zoom control will move your viewpoint back and forth a bit. The bottom slider and arrows move between track segments. Information about the current segment is shown.

> **NEW** Fly-through controls on the right hand pane: Reset, Play/Pause, Speed. Have fun with that. Works on Third person view also.

**Third person** focuses on track _points_ and lets you fly around the current point.  An orange disc will indicate the position on the track. The slider and arrows move to other track points. Information about the current track point is shown. On the right hand side, you can click "Gradient smoother" to smooth dodgy gradients.

> **Gradient smoother** will show (at the bottom right) a list of track points where the gradient suddenly changes by more than the threshold (you set this with the slider). You may click on this list to move directly to that place in the view.
To smooth a section of road, "Drop marker" places a purple cone on the track; then move your current position (backwards or forwards) so that you 'bracket' a section of road. The big button will then let you 'smooth' this part of the road. You choose how bumpy you would like it: 0 is totally smooth, 1 is basically unchanged. You can experiment, and easily "Undo".

**About** -- that's this message.

Click the blue button at the page top to choose a file.

> _Peter Ward, 2020_
"""


viewPointCloud : ScalingInfo -> Model -> Element Msg
viewPointCloud scale model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0.0 0.0 0.0
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters <| distanceFromZoom scale model.zoomLevelOverview
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
                            [ text <| showDecimal2 summary.highestMetres
                            , text <| showDecimal2 summary.lowestMetres
                            , text <| showDecimal2 summary.trackLength
                            , text <| showDecimal2 summary.climbingDistance
                            , text <| showDecimal2 summary.totalClimbing
                            , text <| showDecimal2 summary.descendingDistance
                            , text <| showDecimal2 summary.totalDescending
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
                    , clipDepth = Length.meters (1.0 * scale.metresToClipSpace)
                    , entities = model.entities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        , column []
            [ showSummary
            , viewOptions model
            ]
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


viewFirstPerson scale model =
    let
        getRoad : Maybe DrawingRoad
        getRoad =
            -- N.B. will fail on last node.
            case model.currentNode of
                Just n ->
                    Array.get n model.roadArray

                _ ->
                    Nothing

        summaryData road =
            row [ padding 20 ]
                [ column [ spacing 10 ]
                    [ text "Start point index "
                    , text "Start latitude "
                    , text "Start longitude "
                    , text "Start elevation "
                    , text "Start distance "
                    , text "End latitude "
                    , text "End longitude "
                    , text "End elevation "
                    , text "End distance "
                    , text "Length "
                    , text "Gradient "
                    , text "Bearing "
                    ]
                , column [ spacing 10 ]
                    [ text <| String.fromInt <| Maybe.withDefault 1 model.currentNode
                    , text <| showDecimal6 road.startsAt.trackPoint.lat
                    , text <| showDecimal6 road.startsAt.trackPoint.lon
                    , text <| showDecimal2 road.startsAt.trackPoint.ele
                    , text <| showDecimal2 road.startDistance
                    , text <| showDecimal6 road.endsAt.trackPoint.lat
                    , text <| showDecimal6 road.endsAt.trackPoint.lon
                    , text <| showDecimal2 road.endsAt.trackPoint.ele
                    , text <| showDecimal2 road.endDistance
                    , text <| showDecimal2 road.length
                    , text <| showDecimal2 road.gradient
                    , text <| bearingToDisplayDegrees road.bearing
                    ]
                ]
    in
    case getRoad of
        Nothing ->
            none

        Just road ->
            row []
                [ zoomSlider model.zoomLevelFirstPerson ZoomLevelFirstPerson
                , column []
                    [ viewRoadSegment scale model road
                    , positionControls model
                    ]
                , column [ alignTop, padding 20, spacing 10 ]
                    [ summaryData road
                    , flythroughControls model
                    ]
                ]


flythroughControls : Model -> Element Msg
flythroughControls model =
    let
        flythroughSpeedSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = SetFlythroughSpeed
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Fly-through speed = "
                                ++ (showDecimal2 <|
                                        10.0
                                            ^ model.flythroughSpeed
                                   )
                                ++ " m/sec"
                , min = 1.0 -- i.e. 1
                , max = 3.0 -- i.e. 1000
                , step = Nothing
                , value = model.flythroughSpeed
                , thumb = Input.defaultThumb
                }

        resetButton =
            button
                prettyButtonStyles
                { onPress = Just ResetFlythrough
                , label = el [ Font.size 24 ] <| text "⏮️"
                }

        playButton =
            button
                prettyButtonStyles
                { onPress = Just (RunFlythrough True)
                , label = el [ Font.size 24 ] <| text "▶️"
                }

        pauseButton =
            button
                prettyButtonStyles
                { onPress = Just (RunFlythrough False)
                , label = el [ Font.size 24 ] <| text "⏸"
                }

        playPauseButton =
            case model.flythrough of
                Nothing ->
                    playButton

                Just flying ->
                    if flying.running then
                        pauseButton

                    else
                        playButton

        flythroughPosition =
            case model.flythrough of
                Just fly ->
                    text <| showDecimal2 fly.metresFromRouteStart

                Nothing ->
                    none
    in
    row [ padding 10, spacing 10 ]
        [ resetButton
        , playPauseButton
        , flythroughSpeedSlider

        --, flythroughPosition
        ]


bearingToDisplayDegrees x =
    showDecimal2 <|
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


viewRoadSegment : ScalingInfo -> Model -> DrawingRoad -> Element Msg
viewRoadSegment scale model road =
    let
        eyeHeight =
            -- Helps to be higher up.
            2.0 * scale.metresToClipSpace

        eyePoint =
            case model.flythrough of
                Nothing ->
                    Point3d.meters
                        road.startsAt.x
                        road.startsAt.y
                        (road.startsAt.z + eyeHeight)

                Just flying ->
                    let
                        ( x, y, z ) =
                            flying.cameraPosition
                    in
                    Point3d.meters
                        x
                        y
                        (z + eyeHeight)

        cameraViewpoint =
            case model.flythrough of
                Nothing ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint =
                            Point3d.meters
                                road.endsAt.x
                                road.endsAt.y
                                (road.endsAt.z + eyeHeight)
                        , upDirection = Direction3d.positiveZ
                        }

                Just flying ->
                    let
                        r =
                            flying.segment

                        ( focusX, focusY, focusZ ) =
                            flying.focusPoint
                    in
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint =
                            Point3d.meters focusX focusY (focusZ + eyeHeight)
                        , upDirection = Direction3d.positiveZ
                        }

        camera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees <| 120.0 / model.zoomLevelFirstPerson
                }
    in
    el [] <|
        html <|
            Scene3d.sunny
                { camera = camera
                , dimensions = ( Pixels.int 800, Pixels.int 500 )
                , background = Scene3d.backgroundColor Color.lightBlue
                , clipDepth = Length.meters (1.0 * scale.metresToClipSpace)
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


viewThirdPerson : ScalingInfo -> Model -> Element Msg
viewThirdPerson scale model =
    -- Let's the user spin around and zoom in on selected road point.
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
                    [ viewCurrentNode scale model node
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
                , Input.optionWith ShowGradientFixes <| radioButton Last "Gradient\nsmoother"

                --, Input.optionWith ShowBendFixes <| radioButton Last "Bend\nsmoother"
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
        , showCircle model.smoothedBend
        , bendSmoothnessSlider model
        , viewBearingChanges model
        ]


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


showCircle : Maybe SmoothedBend -> Element Msg
showCircle hello =
    case hello of
        Just sb ->
            let
                ( x, y ) =
                    sb.centre
            in
            column [ Border.width 1 ]
                [ text <| String.fromFloat x
                , text <| String.fromFloat y
                , text <| String.fromFloat <| metresPerDegreeLatitude * sb.radius
                ]

        Nothing ->
            none


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
        gradientSmoothControls =
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
                            [ button
                                prettyButtonStyles
                                { onPress = Just <| SmoothGradient start finish gradient
                                , label =
                                    text <|
                                        "Smooth between markers\nAverage gradient "
                                            ++ showDecimal2 gradient
                                }
                            , smoothnessSlider model
                            ]

                        _ ->
                            []

                ( Just c, Nothing ) ->
                    [ button
                        prettyButtonStyles
                        { onPress = Just (VerticalNodeSpilt c)
                        , label = text "Smooth this transition by\nreplacing with two nodes"
                        }
                    ]

                _ ->
                    []
    in
    column [ spacing 10 ] <|
        [ markerButton model ]
            ++ gradientSmoothControls
            ++ [ undoButton model
               , viewGradientChanges model
               ]


smoothnessSlider : Model -> Element Msg
smoothnessSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetBumpinessFactor
        , label =
            Input.labelBelow [] <|
                text <|
                    "Bumpiness factor = "
                        ++ showDecimal2 model.bumpinessFactor
        , min = 0.0
        , max = 1.0
        , step = Nothing
        , value = model.bumpinessFactor
        , thumb = Input.defaultThumb
        }


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
            column [ padding 20, spacing 20 ]
                [ row [ padding 20 ]
                    [ column [ spacing 10 ]
                        [ text "Index "
                        , text "Latitude "
                        , text "Longitude "
                        , text "Elevation "
                        ]
                    , column [ spacing 10 ]
                        [ text <| String.fromInt <| getNodeNum
                        , text <| showDecimal6 node.trackPoint.lat
                        , text <| showDecimal6 node.trackPoint.lon
                        , text <| showDecimal2 node.trackPoint.ele
                        ]
                    ]
                , flythroughControls model
                ]

        Nothing ->
            none


distanceFromZoom : ScalingInfo -> Float -> Float
distanceFromZoom scale zoomLevel =
    1.0 * scale.metresToClipSpace * 10 ^ (5.0 - zoomLevel)


meanPositionOfNearbyNodes : DrawingNode -> Model -> ( Float, Float, Float )
meanPositionOfNearbyNodes node model =
    -- Probably should not do this in the View.
    let
        eachSide =
            2

        nearbyNodes =
            model.nodes |> drop (node.trackPoint.idx - eachSide) |> take (2 * eachSide)

        acc =
            { count = 0.0, x = 0.0, y = 0.0, z = 0.0 }

        accumulated =
            List.foldl adder acc nearbyNodes

        adder a b =
            { b
                | count = b.count + 1.0
                , x = b.x + a.x
                , y = b.y + a.y
                , z = b.z + a.z
            }
    in
    ( accumulated.x / accumulated.count
    , accumulated.y / accumulated.count
    , accumulated.z / accumulated.count
    )


viewCurrentNode : ScalingInfo -> Model -> DrawingNode -> Element Msg
viewCurrentNode scale model node =
    let
        focus =
            case model.flythrough of
                Just fly ->
                    let
                        ( x, y, z ) =
                            fly.cameraPosition
                    in
                    Point3d.meters x y z

                Nothing ->
                    let
                        ( x, y, z ) =
                            meanPositionOfNearbyNodes node model
                    in
                    Point3d.meters x y z

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = focus
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance =
                            Length.meters <|
                                distanceFromZoom scale model.zoomLevelThirdPerson
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
                    , clipDepth = Length.meters (1.0 * scale.metresToClipSpace)
                    , entities = model.entities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
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
    Time.every 10 Tick


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
