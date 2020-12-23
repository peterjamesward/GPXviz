module Main exposing (main)

import Html.Events.Extra.Wheel
import About exposing (viewAboutText)
import Accordion exposing (..)
import Angle exposing (Angle, inDegrees)
import Array exposing (Array)
import BendSmoother exposing (SmoothedBend, bendIncircle)
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Camera3d
import Color
import Direction3d exposing (negativeZ, positiveY, positiveZ)
import DisplayOptions exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Flythrough exposing (Flythrough, eyeHeight, flythrough)
import Geometry101
import Html.Attributes exposing (id)
import Iso8601
import Json.Decode as E exposing (decodeValue, field, float)
import Length
import List exposing (drop, take)
import MapController exposing (MapInfo, MapState(..), mapPort, mapStopped, messageReceiver, msgDecoder)
import Msg exposing (..)
import NodesAndRoads exposing (..)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Scene3d exposing (Entity)
import Spherical exposing (metresPerDegree)
import Task
import Terrain exposing (makeTerrain)
import Time
import TrackPoint exposing (..)
import Utils exposing (..)
import Vector2d
import Vector3d
import ViewElements exposing (..)
import ViewTypes exposing (..)
import Viewpoint3d
import VisualEntities exposing (..)
import WriteGPX exposing (decimals6, writeGPX)


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
    , currentNode : Int
    , markedNode : Maybe Int
    }


type Loopiness
    = NotALoop Float
    | IsALoop
    | AlmostLoop Float -- if, say, less than 200m back to start.


type alias Model =
    { gpx : Maybe String
    , filename : Maybe String
    , trackName : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , timeOfLastSave : Time.Posix
    , gpxUrl : String
    , trackPoints : List TrackPoint
    , trackPointBox : BoundingBox3d Length.Meters GPXCoords
    , nodeBox : BoundingBox3d Length.Meters LocalCoords
    , nodes : List DrawingNode
    , roads : List DrawingRoad
    , azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Maybe Point -- Capture mouse down position (when clicking on the 3D control)
    , staticVisualEntities : List (Entity LocalCoords) -- our 3D world
    , staticProfileEntities : List (Entity LocalCoords) -- an unrolled 3D world for the profile view.
    , varyingVisualEntities : List (Entity LocalCoords) -- current position and marker node.
    , varyingProfileEntities : List (Entity LocalCoords)
    , terrainEntities : List (Entity LocalCoords)
    , mapVisualEntities : List (Entity LocalCoords) -- for map image only
    , currentNode : Int
    , markedNode : Maybe Int
    , viewingMode : ViewingMode
    , summary : Maybe SummaryData
    , nodeArray : Array DrawingNode
    , roadArray : Array DrawingRoad
    , zoomLevelOverview : Float
    , zoomLevelFirstPerson : Float
    , zoomLevelThirdPerson : Float
    , zoomLevelProfile : Float
    , zoomLevelPlan : Float
    , displayOptions : DisplayOptions
    , abruptGradientChanges : List AbruptChange -- change in gradient exceeds user's threshold
    , abruptBearingChanges : List AbruptChange -- change in gradient exceeds user's threshold
    , zeroLengths : List DrawingRoad -- segments that should not be here.
    , gradientChangeThreshold : Float
    , bearingChangeThreshold : Int
    , hasBeenChanged : Bool
    , smoothingEndIndex : Maybe Int
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , smoothedBend : Maybe SmoothedBend -- computed track points
    , smoothedRoads : List DrawingRoad -- derived road from above,
    , numLineSegmentsForBend : Int
    , bumpinessFactor : Float -- 0.0 => average gradient, 1 => original gradients
    , flythroughSpeed : Float
    , flythrough : Maybe Flythrough
    , loopiness : Loopiness
    , nudgeValue : Float
    , nudgedNodeRoads : List DrawingRoad
    , nudgedRegionStart : Int -- so we can correlate nudgedNodeRoads in profile view (ugh).
    , verticalNudgeValue : Float
    , accordion : List (AccordionEntry Msg)
    , maxSegmentSplitSize : Float
    , mapInfo : Maybe MapController.MapInfo
    , deltaY : Float -- testing mouse wheel events
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gpx = Nothing
      , filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , timeOfLastSave = Time.millisToPosix 0
      , gpxUrl = ""
      , trackPoints = []
      , trackPointBox = BoundingBox3d.singleton Point3d.origin
      , nodeBox = BoundingBox3d.singleton Point3d.origin
      , nodes = []
      , roads = []
      , trackName = Nothing
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = Nothing
      , staticVisualEntities = []
      , varyingVisualEntities = []
      , staticProfileEntities = []
      , varyingProfileEntities = []
      , mapVisualEntities = []
      , terrainEntities = []
      , currentNode = 0
      , markedNode = Nothing
      , viewingMode = AboutView
      , summary = Nothing
      , nodeArray = Array.empty
      , roadArray = Array.empty
      , zoomLevelOverview = 1.0
      , zoomLevelFirstPerson = 1.0
      , zoomLevelThirdPerson = 2.0
      , zoomLevelProfile = 1.0
      , zoomLevelPlan = 1.0
      , displayOptions = defaultDisplayOptions
      , abruptGradientChanges = []
      , abruptBearingChanges = []
      , zeroLengths = []
      , gradientChangeThreshold = 10.0 -- Note, this is not an angle, it's a percentage (tangent).
      , bearingChangeThreshold = 90
      , hasBeenChanged = False
      , smoothingEndIndex = Nothing
      , undoStack = []
      , redoStack = []
      , smoothedBend = Nothing
      , smoothedRoads = []
      , numLineSegmentsForBend = 3
      , bumpinessFactor = 0.0
      , flythrough = Nothing
      , flythroughSpeed = 1.0
      , loopiness = NotALoop 0.0
      , nudgeValue = 0.0
      , nudgedNodeRoads = []
      , nudgedRegionStart = 0
      , verticalNudgeValue = 0.0
      , accordion = []
      , maxSegmentSplitSize = 30.0 -- When we split a segment, how close should the track points be.
      , mapInfo = Nothing
      , deltaY = 0.0
      }
    , Task.perform AdjustTimeZone Time.here
    )


genericAccordion model =
    [ { label = "File"
      , state = Expanded
      , content = overviewSummary model
      }
    , { label = "Road data"
      , state = Contracted
      , content = summaryData (lookupRoad model model.currentNode)
      }
    , { label = "Visual styles"
      , state = Contracted
      , content = viewOptions model
      }
    , { label = "Loop maker"
      , state = Contracted
      , content = viewLoopTools model
      }
    , { label = "Fly-through"
      , state = Contracted
      , content = flythroughControls model
      }
    , { label = "Smooth gradient"
      , state = Contracted
      , content = viewGradientFixerPane model
      }
    , { label = "Nudge node"
      , state = Contracted
      , content = viewNudgeTools model
      }
    , { label = "Smooth bend"
      , state = Contracted
      , content = viewBendFixerPane model
      }
    , { label = "Straighten"
      , state = Contracted
      , content = viewStraightenTools model
      }
    , { label = "Trackpoints"
      , state = Contracted
      , content = viewTrackPointTools model
      }
    , { label = "Gradient problems"
      , state = Contracted
      , content = viewGradientChanges model
      }
    , { label = "Bend problems"
      , state = Contracted
      , content = viewBearingChanges model
      }
    , { label = "Map Info"
      , state = Contracted
      , content = MapController.viewMapInfo model.mapInfo
      }
    ]


initialiseAccordion model =
    { model
        | accordion = genericAccordion model
    }


addToUndoStack : String -> Model -> Model
addToUndoStack label model =
    { model
        | undoStack =
            { label = label
            , trackPoints = model.trackPoints
            , currentNode = model.currentNode
            , markedNode = model.markedNode
            }
                :: List.take 9 model.undoStack
        , redoStack = []
    }


clearTheModel model =
    { model
        | gpx = Nothing
        , timeOfLastSave = Time.millisToPosix 0
        , trackPoints = []
        , trackPointBox = BoundingBox3d.singleton Point3d.origin
        , nodeBox = BoundingBox3d.singleton Point3d.origin
        , nodes = []
        , roads = []
        , trackName = Nothing
        , orbiting = Nothing
        , staticVisualEntities = []
        , varyingVisualEntities = []
        , staticProfileEntities = []
        , varyingProfileEntities = []
        , terrainEntities = []
        , currentNode = 0
        , markedNode = Nothing
        , summary = Nothing
        , nodeArray = Array.empty
        , roadArray = Array.empty
        , abruptGradientChanges = []
        , abruptBearingChanges = []
        , zeroLengths = []
        , hasBeenChanged = False
        , smoothingEndIndex = Nothing
        , undoStack = []
        , redoStack = []
        , smoothedBend = Nothing
        , smoothedRoads = []
        , flythrough = Nothing
        , loopiness = NotALoop 0.0
        , nudgedNodeRoads = []
    }


locallyHandleMapMessage : Model -> E.Value -> Model
locallyHandleMapMessage model json =
    -- So we don't need to keep going to the MapController.
    -- These will be Model-domain messages.
    let
        msg =
            decodeValue msgDecoder json

        lat =
            decodeValue (field "lat" float) json

        lon =
            decodeValue (field "lon" float) json

        zoom =
            decodeValue (field "zoom" float) json
    in
    case msg of
        Ok "click" ->
            --{ 'msg' : 'click'
            --, 'lat' : e.lat()
            --, 'lon' : e.lon()
            --} );
            case ( lat, lon ) of
                ( Ok lat1, Ok lon1 ) ->
                    makeNearestNodeCurrent model lon1 lat1
                        |> tryBendSmoother

                _ ->
                    model

        _ ->
            model


makeNearestNodeCurrent : Model -> Float -> Float -> Model
makeNearestNodeCurrent model lon lat =
    -- Searching like this may be too slow. Wait and see.
    let
        nearbyPoints =
            List.sortBy distance
                model.roads

        distance point =
            Geometry101.distance
                { x = point.startsAt.trackPoint.lon, y = point.startsAt.trackPoint.lat }
                { x = lon, y = lat }
    in
    case nearbyPoints of
        n :: _ ->
            { model | currentNode = n.index }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        options =
            model.displayOptions
    in
    case msg of
        MapMessage jsonMsg ->
            case model.mapInfo of
                Just mapInfo ->
                    case MapController.processMapMessage mapInfo jsonMsg of
                        Just ( newInfo, cmd ) ->
                            ( { model | mapInfo = Just newInfo }
                            , cmd
                            )

                        Nothing ->
                            let
                                newModel =
                                    locallyHandleMapMessage model jsonMsg
                                        |> deriveVaryingVisualEntities
                            in
                            ( newModel
                            , updateMapVaryingElements newModel
                            )

                Nothing ->
                    ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }
                |> advanceFlythrough newTime
            , Cmd.none
            )

        AccordionMessage entry ->
            ( { model | accordion = accordionToggle model.accordion entry }
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
            -- TODO: Tidy up the removal of zero length segments,
            -- so as not to repeat ourselves here.
            let
                newModel =
                    model
                        |> clearTheModel
                        |> parseGPXintoModel content
                        |> deriveNodesAndRoads
                        |> deriveProblems
                        |> deleteZeroLengthSegments
                        |> deriveNodesAndRoads
                        |> deriveProblems
                        |> clearTerrain
                        |> initialiseAccordion
                        |> deriveStaticVisualEntities
                        |> deriveVaryingVisualEntities
                        |> resetViewSettings
            in
            switchViewMode newModel newModel.viewingMode

        UserMovedNodeSlider node ->
            let
                newModel =
                    { model | currentNode = node }
                        |> cancelFlythrough
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        SetSmoothingEnd idx ->
            let
                newModel =
                    { model | smoothingEndIndex = Just idx }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        PositionForwardOne ->
            let
                newModel =
                    { model
                        | currentNode = modBy (List.length model.nodes) (model.currentNode + 1)
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
                        |> cancelFlythrough
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        PositionBackOne ->
            let
                newModel =
                    { model
                        | currentNode = modBy (List.length model.nodes) (model.currentNode - 1)
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
                        |> cancelFlythrough
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        MarkerForwardOne ->
            let
                newModel =
                    { model
                        | markedNode =
                            Maybe.map
                                (\m -> modBy (List.length model.nodes) (m + 1))
                                model.markedNode
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        MarkerBackOne ->
            let
                newModel =
                    { model
                        | markedNode =
                            Maybe.map
                                (\m -> modBy (List.length model.nodes) (m - 1))
                                model.markedNode
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        SetMaxTurnPerSegment turn ->
            let
                newModel =
                    { model
                        | numLineSegmentsForBend = turn
                    }
                        |> tryBendSmoother
                        --|> deriveStaticVisualEntities
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        ChooseViewMode mode ->
            switchViewMode model mode

        MapRemoved _ ->
            case model.mapInfo of
                Just info ->
                    let
                        newInfo =
                            { info | mapState = MapStopped }
                    in
                    ( { model
                        | viewingMode = info.nextView
                        , mapInfo = Just newInfo
                      }
                        |> deriveVaryingVisualEntities
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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

        ZoomLevelProfile level ->
            ( { model | zoomLevelProfile = level }
            , Cmd.none
            )

        ZoomLevelPlan level ->
            ( { model | zoomLevelPlan = level }
            , Cmd.none
            )

        ImageGrab ( dx, dy ) ->
            -- Mouse behaviour depends which view is in use...
            -- Need to add intersection test for Profile & Plan views.
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
            { model
                | displayOptions = { options | roadCones = not options.roadCones }
            }
                |> deriveStaticVisualEntities
                |> synchroniseMap

        TogglePillars _ ->
            { model
                | displayOptions = { options | roadPillars = not options.roadPillars }
            }
                |> deriveStaticVisualEntities
                |> synchroniseMap

        ToggleRoad _ ->
            { model
                | displayOptions = { options | roadTrack = not options.roadTrack }
            }
                |> deriveStaticVisualEntities
                |> synchroniseMap

        ToggleCentreLine _ ->
            { model
                | displayOptions = { options | centreLine = not options.centreLine }
            }
                |> deriveStaticVisualEntities
                |> synchroniseMap

        SetCurtainStyle style ->
            { model
                | displayOptions = { options | curtainStyle = style }
            }
                |> deriveStaticVisualEntities
                |> synchroniseMap

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
            deleteZeroLengthSegments model
                |> trackHasChanged

        OutputGPX ->
            ( { model | hasBeenChanged = False }
            , outputGPX model
            )

        SmoothGradient g ->
            smoothGradient model g
                |> trackHasChanged

        SmoothBend ->
            model
                |> smoothBend
                |> trackHasChanged

        Undo ->
            case model.undoStack of
                action :: undos ->
                    { model
                        | trackPoints = action.trackPoints
                        , undoStack = undos
                        , redoStack = { action | trackPoints = model.trackPoints } :: model.redoStack
                        , currentNode = action.currentNode
                        , markedNode = action.markedNode
                    }
                        |> trackHasChanged

                _ ->
                    ( model, Cmd.none )

        Redo ->
            case model.redoStack of
                action :: redos ->
                    { model
                        | trackPoints = action.trackPoints
                        , redoStack = redos
                        , undoStack = { action | trackPoints = model.trackPoints } :: model.undoStack
                        , currentNode = action.currentNode
                        , markedNode = action.markedNode
                    }
                        |> trackHasChanged

                _ ->
                    ( model, Cmd.none )

        ToggleMarker ->
            let
                newModel =
                    { model
                        | markedNode =
                            case model.markedNode of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    Just model.currentNode
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
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

        InsertBeforeOrAfter node direction ->
            insertTrackPoint node model
                |> trackHasChanged

        DeleteCurrentPoint c ->
            deleteTrackPoint c model
                |> trackHasChanged

        ChangeLoopStart c ->
            changeLoopStart c model
                |> trackHasChanged

        MakeTerrain ->
            ( deriveTerrain model
            , Cmd.none
            )

        ClearTerrain ->
            ( clearTerrain model
            , Cmd.none
            )

        CloseTheLoop ->
            closeTheLoop model
                |> trackHasChanged

        ReverseTrack ->
            reverseTrack model
                |> trackHasChanged

        StraightenStraight ->
            straightenStraight model
                |> trackHasChanged

        SetHorizontalNudgeFactor horizontal ->
            let
                newModel =
                    simulateNudgeNode model horizontal model.verticalNudgeValue
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        SetVerticalNudgeFactor vertical ->
            let
                newModel =
                    simulateNudgeNode model model.nudgeValue vertical
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        NudgeNode horizontal vertical ->
            nudgeNode model horizontal vertical
                |> trackHasChanged

        SplitRoad ->
            model |> splitRoad |> trackHasChanged

        SetMaxTrackpointSpacing f ->
            ( { model | maxSegmentSplitSize = f }
            , Cmd.none
            )

        MouseWheel deltaY ->
            let
                factor = -0.001
            in
            ( case model.viewingMode of
                FirstPersonView ->
                    { model | zoomLevelFirstPerson = model.zoomLevelFirstPerson + deltaY * factor }
                ThirdPersonView ->
                    { model | zoomLevelThirdPerson = model.zoomLevelThirdPerson + deltaY * factor }
                ProfileView ->
                    { model | zoomLevelProfile = model.zoomLevelProfile + deltaY * factor }
                PlanView ->
                    { model | zoomLevelPlan = model.zoomLevelPlan + deltaY * factor }
                _ -> model
            , Cmd.none
            )


trackHasChanged model =
    model
        |> deriveNodesAndRoads
        |> deriveStaticVisualEntities
        |> deriveProblems
        |> clearTerrain
        |> deriveVaryingVisualEntities
        |> synchroniseMap


updateMapVaryingElements : Model -> Cmd Msg
updateMapVaryingElements model =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        currentNode =
            Array.get model.currentNode model.nodeArray

        markedNode =
            Array.get marker model.nodeArray

        nudgedTrackPoints =
            List.map (.startsAt >> .trackPoint) (List.take 1 model.nudgedNodeRoads)
                ++ List.map (.endsAt >> .trackPoint) model.nudgedNodeRoads
    in
    case ( currentNode, markedNode ) of
        ( Just node1, Just node2 ) ->
            MapController.addMarkersToMap
                ( node1.trackPoint.lon, node1.trackPoint.lat )
                (Just ( node2.trackPoint.lon, node2.trackPoint.lat ))
                (Maybe.withDefault [] <| Maybe.map .trackPoints model.smoothedBend)
                nudgedTrackPoints

        ( Just node1, Nothing ) ->
            MapController.addMarkersToMap
                ( node1.trackPoint.lon, node1.trackPoint.lat )
                Nothing
                (Maybe.withDefault [] <| Maybe.map .trackPoints model.smoothedBend)
                nudgedTrackPoints

        _ ->
            Cmd.none


synchroniseMap : Model -> ( Model, Cmd Msg )
synchroniseMap model =
    -- This may not be optimal.
    switchViewMode model model.viewingMode


switchViewMode : Model -> ViewingMode -> ( Model, Cmd Msg )
switchViewMode model mode =
    -- When changing to the map, we must use ports to drive the new map.
    -- When changing *from* the map, we must first remove the map (before we cxan destroy the DIV).
    -- So this need some careful structuring.
    let
        current =
            Array.get model.currentNode model.nodeArray

        marked =
            case model.markedNode of
                Just m ->
                    Array.get m model.nodeArray

                Nothing ->
                    Nothing

        newMapInfo box =
            { mapState = WaitingForNode
            , box = box
            , points = model.trackPoints
            , nextView = MapView
            , centreLon = Length.inMeters <| BoundingBox3d.midX box
            , centreLat = Length.inMeters <| BoundingBox3d.midY box
            , mapZoom = 12.0
            , current =
                case current of
                    Just m ->
                        ( m.trackPoint.lon, m.trackPoint.lat )

                    Nothing ->
                        ( 0.0, 0.0 )
            , marker =
                case marked of
                    Just m ->
                        Just ( m.trackPoint.lon, m.trackPoint.lat )

                    Nothing ->
                        Nothing
            }

        updatedMapInfo info =
            { info
                | current =
                    case current of
                        Just m ->
                            ( m.trackPoint.lon, m.trackPoint.lat )

                        Nothing ->
                            ( 0.0, 0.0 )
                , marker =
                    case marked of
                        Just m ->
                            Just ( m.trackPoint.lon, m.trackPoint.lat )

                        Nothing ->
                            Nothing
            }
    in
    case ( model.viewingMode, mode ) of
        ( MapView, MapView ) ->
            -- Not changing mode, may need to refresh track, wait and see.
            case model.mapInfo of
                Just info ->
                    ( model
                    , MapController.addTrackToMap info
                    )

                Nothing ->
                    -- Should not occur, mapView without a mapInfo!
                    ( model, Cmd.none )

        ( _, MapView ) ->
            -- Switch to map view
            case model.mapInfo of
                Just mapInfo ->
                    -- We have a residual map state, we need to restore the map in correct position.
                    let
                        newInfo =
                            updatedMapInfo mapInfo

                        newModel =
                            { model
                                | viewingMode = mode
                                , mapInfo = Just newInfo
                            }
                    in
                    ( newModel
                    , MapController.createMap newInfo
                    )

                Nothing ->
                    -- If the map state is Stopped, need to wait for DOM node before creating map.
                    let
                        newModel =
                            { model
                                | viewingMode = mode
                                , mapInfo = Just (newMapInfo model.trackPointBox)
                            }
                                |> deriveVaryingVisualEntities
                    in
                    ( newModel
                    , MapController.createMap (newMapInfo newModel.trackPointBox)
                    )

        ( MapView, _ ) ->
            -- Request map removal, do not destroy the DOM node yet.
            let
                changedMapInfo info =
                    { info
                        | mapState = MapStopping
                        , nextView = mode
                    }
            in
            ( { model | mapInfo = Maybe.map changedMapInfo model.mapInfo }
                |> deriveVaryingVisualEntities
            , MapController.removeMap
            )

        ( _, _ ) ->
            -- Map not involved, happy days.
            ( { model | viewingMode = mode } |> deriveVaryingVisualEntities
            , Cmd.none
            )


nudgeTrackPoint : TrackPoint -> Float -> Float -> Float -> TrackPoint
nudgeTrackPoint baseTP roadBearing horizontal vertical =
    let
        roadVector =
            -- The negation because, no idea.
            Vector2d.rTheta (Length.meters 1.0)
                (Angle.radians <| -1.0 * roadBearing)
                |> Vector2d.rotateClockwise

        nudgeVector =
            Vector2d.perpendicularTo roadVector
                |> Vector2d.scaleBy (horizontal / metresPerDegree)

        trackPoint2d =
            Point2d.meters baseTP.lon baseTP.lat

        nudgedTrackPoint2d =
            Point2d.translateBy nudgeVector trackPoint2d
    in
    { baseTP
        | lat = Length.inMeters <| Point2d.yCoordinate nudgedTrackPoint2d
        , lon = Length.inMeters <| Point2d.xCoordinate nudgedTrackPoint2d
        , ele = baseTP.ele + vertical
    }


simulateNudgeNode : Model -> Float -> Float -> Model
simulateNudgeNode model horizontal vertical =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( firstNudgeNode, lastNudgeNode ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )
    in
    simulateNodeRangeNudge model firstNudgeNode lastNudgeNode horizontal vertical


simulateNodeRangeNudge : Model -> Int -> Int -> Float -> Float -> Model
simulateNodeRangeNudge model node1 nodeN horizontal vertical =
    let
        targetNodes =
            List.drop node1 <| List.take (nodeN + 1) model.nodes

        getBearingForNode : DrawingNode -> Float
        getBearingForNode node =
            -- We need roads only to get the bearing that we use to nudge sideways.
            if node.trackPoint.idx < Array.length model.roadArray then
                Array.get node.trackPoint.idx model.roadArray
                    |> Maybe.map .bearing
                    |> Maybe.withDefault 0.0

            else
                Array.get (Array.length model.roadArray - 1) model.roadArray
                    |> Maybe.map .bearing
                    |> Maybe.withDefault 0.0

        unmovedEndPoint =
            -- Only if we are not at the end of the track.
            case Array.get (nodeN + 1) model.nodeArray of
                Just endNode ->
                    [ endNode.trackPoint ]

                Nothing ->
                    []

        prevNode =
            Array.get (node1 - 1) model.nodeArray

        nudgedStartPoints =
            List.map
                (\node ->
                    nudgeTrackPoint
                        node.trackPoint
                        (getBearingForNode node)
                        horizontal
                        vertical
                )
                targetNodes

        nudgedListForVisuals =
            (case prevNode of
                Nothing ->
                    []

                Just prev ->
                    [ prev.trackPoint ]
            )
                ++ nudgedStartPoints
                ++ unmovedEndPoint
    in
    { model
        | nudgeValue = horizontal
        , verticalNudgeValue = vertical
        , nudgedRegionStart = node1
        , nudgedNodeRoads =
            deriveRoads <|
                deriveNodes model.trackPointBox <|
                    nudgedListForVisuals
    }


nudgeNode : Model -> Float -> Float -> Model
nudgeNode model horizontal vertical =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( firstNudgeNode, lastNudgeNode ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )
    in
    nudgeNodeRange model firstNudgeNode lastNudgeNode horizontal vertical


nudgeNodeRange : Model -> Int -> Int -> Float -> Float -> Model
nudgeNodeRange model node1 nodeN horizontal vertical =
    -- Apply the nudge factor permanently.
    -- TODO: Serious code duplication going on here.
    let
        undoMessage =
            if nodeN > node1 then
                "Nudge " ++ String.fromInt node1 ++ " to " ++ String.fromInt nodeN

            else
                "Nudge node " ++ String.fromInt node1

        targetNodes =
            List.drop node1 <| List.take (nodeN + 1) model.nodes

        getBearingForNode : DrawingNode -> Float
        getBearingForNode node =
            -- We need roads only to get the bearing that we use to nudge sideways.
            if node.trackPoint.idx < Array.length model.roadArray then
                Array.get node.trackPoint.idx model.roadArray
                    |> Maybe.map .bearing
                    |> Maybe.withDefault 0.0

            else
                Array.get (Array.length model.roadArray - 1) model.roadArray
                    |> Maybe.map .bearing
                    |> Maybe.withDefault 0.0

        unmovedEndPoint =
            -- Only if we are not at the end of the track.
            case Array.get (nodeN + 1) model.nodeArray of
                Just endNode ->
                    [ endNode.trackPoint ]

                Nothing ->
                    []

        prevNode =
            Array.get (node1 - 1) model.nodeArray

        nudgedStartPoints =
            List.map
                (\node ->
                    nudgeTrackPoint
                        node.trackPoint
                        (getBearingForNode node)
                        horizontal
                        vertical
                )
                targetNodes

        nudgedListForVisuals =
            (case prevNode of
                Nothing ->
                    []

                Just prev ->
                    [ prev.trackPoint ]
            )
                ++ nudgedStartPoints
                ++ unmovedEndPoint
    in
    addToUndoStack undoMessage model
        |> (\m ->
                { m
                    | trackPoints =
                        List.take node1 model.trackPoints
                            ++ nudgedStartPoints
                            ++ List.drop (nodeN + 1) model.trackPoints
                    , nudgedNodeRoads = []
                    , nudgedRegionStart = 0
                    , nudgeValue = 0.0
                    , verticalNudgeValue = 0.0
                }
           )


splitRoad : Model -> Model
splitRoad model =
    -- Introduce additional trackpoints in all segments **between** markers.
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( startNode, endNode ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )

        undoMessage =
            "Split between " ++ String.fromInt startNode ++ " and " ++ String.fromInt endNode

        newPointsIn segment =
            let
                trackPointsNeeded =
                    -- Including the final one.
                    -- Replacing this should make it easier for the multiple segment case.
                    ceiling <| segment.length / model.maxSegmentSplitSize
            in
            List.map
                (\i ->
                    interpolateSegment
                        (toFloat i / toFloat trackPointsNeeded)
                        segment.startsAt.trackPoint
                        segment.endsAt.trackPoint
                )
                (List.range 1 trackPointsNeeded)

        totalTrackPointsBefore =
            List.length model.trackPoints

        segmentsToInterpolate =
            model.roads |> List.take endNode |> List.drop startNode

        allNewTrackPoints =
            List.concatMap
                newPointsIn
                segmentsToInterpolate

        precedingTrackPoints =
            List.take (startNode + 1) model.trackPoints

        subsequentTrackPoints =
            List.drop (endNode + 1) model.trackPoints

        newTrackPointList =
            precedingTrackPoints ++ allNewTrackPoints ++ subsequentTrackPoints

        makeItSo mdl =
            { mdl
                | trackPoints = reindexTrackpoints newTrackPointList
                , currentNode =
                    if mdl.currentNode == endNode then
                        mdl.currentNode + List.length newTrackPointList - totalTrackPointsBefore

                    else
                        mdl.currentNode
                , markedNode =
                    Maybe.map
                        (\v -> v + List.length newTrackPointList - totalTrackPointsBefore)
                        model.markedNode
            }
    in
    if startNode < endNode then
        model |> addToUndoStack undoMessage |> makeItSo

    else
        model


closeTheLoop : Model -> Model
closeTheLoop model =
    let
        maybeFirstSegment =
            List.head model.roads

        backOneMeter : DrawingRoad -> TrackPoint
        backOneMeter segment =
            let
                newLatLon : Point2d Length.Meters LocalCoords
                newLatLon =
                    Point2d.interpolateFrom
                        (Point2d.meters segment.startsAt.trackPoint.lon segment.startsAt.trackPoint.lat)
                        (Point2d.meters segment.endsAt.trackPoint.lon segment.endsAt.trackPoint.lat)
                        backAmount

                backAmount =
                    -- Set back new point equivalent to 1 meter, but we're working in lat & lon.
                    -- The fraction should be valid.
                    -1.0 / segment.length
            in
            { lat = Length.inMeters <| Point2d.yCoordinate newLatLon
            , lon = Length.inMeters <| Point2d.xCoordinate newLatLon
            , ele = segment.startsAt.trackPoint.ele
            , idx = 0
            }

        newTrack gap segment1 =
            if gap < 1.0 then
                -- replace last trackpoint with the first
                List.reverse <|
                    List.take 1 model.trackPoints
                        ++ (List.drop 1 <| List.reverse model.trackPoints)

            else
                -- A nicer solution here is to put a new trackpoint slightly "behing"
                -- the existing start, and then join the current last trackpoint to
                -- this new one. Existing tools can then be used to smooth as required.
                model.trackPoints
                    ++ [ backOneMeter segment1 ]
                    ++ List.take 1 model.trackPoints
    in
    case ( model.loopiness, maybeFirstSegment ) of
        ( AlmostLoop gap, Just segment1 ) ->
            addToUndoStack "complete loop" model
                |> (\m ->
                        { m | trackPoints = reindexTrackpoints (newTrack gap segment1) }
                   )

        _ ->
            model


clearTerrain : Model -> Model
clearTerrain model =
    { model | terrainEntities = [] }


insertTrackPoint : Int -> Model -> Model
insertTrackPoint n model =
    -- Replace the current node with two close nodes that each have half the gradient change.
    -- 'Close' being perhaps the lesser of one metre and half segment length.
    -- Lat and Lon to be linear interpolation.
    let
        undoMessage =
            "Insert at " ++ String.fromInt n
    in
    case ( Array.get (n - 1) model.roadArray, Array.get n model.roadArray ) of
        ( Just before, Just after ) ->
            let
                amountToStealFromFirstSegment =
                    min 4.0 (before.length / 2.0)

                amountToStealFromSecondSegment =
                    min 4.0 (after.length / 2.0)

                commonAmountToSteal =
                    min amountToStealFromFirstSegment amountToStealFromSecondSegment

                firstTP =
                    interpolateSegment
                        (commonAmountToSteal / before.length)
                        before.endsAt.trackPoint
                        before.startsAt.trackPoint

                secondTP =
                    interpolateSegment
                        (commonAmountToSteal / after.length)
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

                updateModel m =
                    { m
                        | trackPoints = reindexTrackpoints newTPs
                        , currentNode = model.currentNode + 1
                    }
            in
            model
                |> addToUndoStack undoMessage
                |> updateModel

        _ ->
            model


deleteTrackPoint : Int -> Model -> Model
deleteTrackPoint n model =
    let
        undoMessage =
            "Delete track point " ++ String.fromInt n
    in
    let
        precedingTPs =
            model.trackPoints |> List.take n

        remainingTPs =
            model.trackPoints |> List.drop (n + 1)

        newTPs =
            precedingTPs ++ remainingTPs

        makeItSo m =
            { m
                | trackPoints = reindexTrackpoints newTPs
                , currentNode = min n (List.length newTPs - 2)
            }
    in
    model |> addToUndoStack undoMessage |> makeItSo


changeLoopStart : Int -> Model -> Model
changeLoopStart n model =
    let
        undoMessage =
            "move start point to " ++ String.fromInt n
    in
    let
        newStart =
            List.take 1 remainingTPs

        precedingTPs =
            List.take n model.trackPoints

        remainingTPs =
            List.drop n model.trackPoints

        newTPs =
            -- Make sure loop remains closed.
            remainingTPs ++ precedingTPs ++ newStart

        makeItSo m =
            { m
                | trackPoints = reindexTrackpoints newTPs
                , currentNode = 0
            }
    in
    model |> addToUndoStack undoMessage |> makeItSo


reverseTrack : Model -> Model
reverseTrack model =
    let
        undoMessage =
            "reverse track"

        makeItSo m =
            { m
                | trackPoints = reindexTrackpoints <| List.reverse model.trackPoints
                , currentNode = 0
            }
    in
    model |> addToUndoStack undoMessage |> makeItSo


straightenStraight : Model -> Model
straightenStraight model =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( n1, n2 ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )

        ( firstSeg, lastSeg ) =
            ( Array.get n1 model.roadArray
            , Array.get n2 model.roadArray
            )

        undoMessage =
            "straighten from "
                ++ String.fromInt n1
                ++ " to "
                ++ String.fromInt n2
                ++ "."
    in
    case ( firstSeg, lastSeg ) of
        ( Just firstRoad, Just lastRoad ) ->
            let
                startPoint =
                    Point2d.meters
                        firstRoad.startsAt.trackPoint.lon
                        firstRoad.startsAt.trackPoint.lat

                endPoint =
                    Point2d.meters
                        lastRoad.startsAt.trackPoint.lon
                        lastRoad.startsAt.trackPoint.lat

                trackPointsToMove =
                    -- Note, include startTP and it will move by zero,
                    model.trackPoints |> List.take n2 |> List.drop n1

                affectedRoads =
                    model.roads |> List.take n2 |> List.drop n1

                affectedLength =
                    List.sum <| List.map .length affectedRoads

                ( _, cumulativeLengthsReversed ) =
                    List.foldl
                        (\r ( running, acc ) -> ( r.length + running, running :: acc ))
                        ( 0.0, [] )
                        affectedRoads

                newTPs =
                    List.map2
                        interpolateTrackPoint
                        trackPointsToMove
                        (List.reverse cumulativeLengthsReversed)

                interpolateTrackPoint original fraction =
                    let
                        interpolatedPoint =
                            Point2d.interpolateFrom startPoint endPoint (fraction / affectedLength)
                    in
                    { lat = Length.inMeters <| Point2d.yCoordinate interpolatedPoint
                    , lon = Length.inMeters <| Point2d.xCoordinate interpolatedPoint
                    , ele = original.ele
                    , idx = 0
                    }

                splicedTPs =
                    List.take n1 model.trackPoints
                        ++ newTPs
                        ++ List.drop n2 model.trackPoints
            in
            addToUndoStack undoMessage model
                |> (\mdl ->
                        { mdl | trackPoints = reindexTrackpoints splicedTPs }
                   )

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
    case model.flythrough of
        Just flying ->
            { model
                | flythrough =
                    Just <|
                        flythrough
                            newTime
                            flying
                            model.flythroughSpeed
                            model.roads
            }

        Nothing ->
            model


resetFlythrough : Model -> Model
resetFlythrough model =
    let
        segment =
            lookupRoad model model.currentNode
    in
    case segment of
        Just seg ->
            { model
                | flythrough =
                    Just
                        { metresFromRouteStart = seg.startDistance
                        , running = False
                        , cameraPosition =
                            Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 eyeHeight)
                                seg.startsAt.location
                        , focusPoint =
                            Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 eyeHeight)
                                seg.endsAt.location
                        , lastUpdated = model.time
                        , segment = seg
                        }
            }

        Nothing ->
            model


tryBendSmoother : Model -> Model
tryBendSmoother model =
    -- Note we work here in trackpoint space, not node/road space.
    -- This because we will need to create GPX entries, so better to start there IMHO.
    let
        failed =
            { model
                | smoothedBend = Nothing
                , smoothedRoads = []
                , nudgedNodeRoads = []
                , nudgeValue = 0.0
                , verticalNudgeValue = 0.0
            }

        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( n1, n2 ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )

        entrySegment =
            Array.get n1 model.roadArray

        exitSegment =
            Array.get (n2 - 1) model.roadArray
    in
    if n2 >= n1 + 2 then
        case ( entrySegment, exitSegment ) of
            ( Just road1, Just road2 ) ->
                let
                    ( pa, pb ) =
                        ( road1.startsAt.trackPoint
                        , road1.endsAt.trackPoint
                        )

                    ( pc, pd ) =
                        ( road2.startsAt.trackPoint
                        , road2.endsAt.trackPoint
                        )

                    newTrack =
                        bendIncircle model.numLineSegmentsForBend pa pb pc pd
                in
                case newTrack of
                    Just track ->
                        { model
                            | smoothedBend = newTrack
                            , smoothedRoads =
                                deriveRoads <|
                                    deriveNodes model.trackPointBox <|
                                        track.trackPoints
                            , nudgedNodeRoads = []
                            , nudgeValue = 0.0
                            , verticalNudgeValue = 0.0
                        }

                    Nothing ->
                        failed

            _ ->
                failed

    else
        failed


smoothGradient : Model -> Float -> Model
smoothGradient model gradient =
    -- This feels like a simple foldl, creating a new list of TrackPoints
    -- which we then splice into the model.
    -- It's a fold because we must keep track of the current elevation
    -- which will increase with each segment.
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( start, finish ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )

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
            addToUndoStack undoMessage model
                |> (\m ->
                        { m
                            | trackPoints =
                                reindexTrackpoints <|
                                    List.take (start + 1) m.trackPoints
                                        ++ bumpyTrackPoints
                                        ++ List.drop finish m.trackPoints
                        }
                   )

        Nothing ->
            -- shouldn't happen
            model


smoothBend : Model -> Model
smoothBend model =
    -- The replacement bend is a pre-computed list of trackpoints,
    -- so we need only splice them in.
    let
        undoMessage bend =
            "bend smoothing\nfrom "
                ++ String.fromInt bend.startIndex
                ++ " to "
                ++ String.fromInt bend.endIndex
                ++ ", \nradius "
                ++ showDecimal2 bend.radius
                ++ " metres."

        marker =
            Maybe.withDefault model.currentNode model.markedNode
    in
    case model.smoothedBend of
        Just bend ->
            let
                numCurrentPoints =
                    abs (model.currentNode - marker)

                numNewPoints =
                    List.length bend.trackPoints

                newCurrent =
                    if model.currentNode > bend.startIndex then
                        model.currentNode - numCurrentPoints + numNewPoints

                    else
                        model.currentNode

                newMark =
                    if marker > bend.startIndex then
                        marker - numCurrentPoints + numNewPoints

                    else
                        marker

                makeItSo m =
                    { m
                        | trackPoints =
                            reindexTrackpoints <|
                                List.take bend.startIndex m.trackPoints
                                    ++ bend.trackPoints
                                    ++ List.drop bend.endIndex m.trackPoints
                        , smoothedBend = Nothing
                        , smoothedRoads = []
                        , currentNode = newCurrent
                        , markedNode = Just newMark
                    }
            in
            model |> addToUndoStack (undoMessage bend) |> makeItSo

        _ ->
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
    { model
        | gpx = Just content
        , trackName = parseTrackName content
        , trackPoints = parseTrackPoints content
        , hasBeenChanged = False
    }


deriveNodesAndRoads : Model -> Model
deriveNodesAndRoads model =
    let
        trackPointAsPoint tp =
            Point3d.meters tp.lon tp.lat tp.ele

        withNodes m =
            { m | nodes = deriveNodes m.trackPointBox m.trackPoints }

        withTrackPointScaling m =
            { m
                | trackPointBox =
                    -- This is a good reason for the bbox to be a Maybe B~
                    case m.trackPoints of
                        tp1 :: tps ->
                            BoundingBox3d.hull
                                (trackPointAsPoint tp1)
                                (List.map trackPointAsPoint tps)

                        _ ->
                            BoundingBox3d.singleton Point3d.origin
            }

        withNodeScaling m =
            { m
                | nodeBox =
                    case m.nodes of
                        node1 :: nodes ->
                            BoundingBox3d.hull node1.location <| List.map .location nodes

                        _ ->
                            BoundingBox3d.singleton Point3d.origin
            }

        withRoads m =
            let
                roads =
                    deriveRoads m.nodes
            in
            { m
                | roads = roads
            }

        withSummary m =
            { m | summary = Just <| deriveSummary m.roads }

        withArrays m =
            { m
                | nodeArray = Array.fromList m.nodes
                , roadArray = Array.fromList m.roads
            }
    in
    model
        |> withTrackPointScaling
        |> withNodes
        |> withNodeScaling
        |> withRoads
        |> withSummary
        |> withArrays


resetViewSettings : Model -> Model
resetViewSettings model =
    let
        ( x, y, z ) =
            BoundingBox3d.dimensions model.nodeBox

        zoomLevel =
            -- Empirical!
            clamp 1.0 4.0 <|
                5.0
                    - logBase 10 (max (Length.inMeters x) (Length.inMeters y))

        newMapInfo : BoundingBox3d Length.Meters GPXCoords -> MapInfo -> MapInfo
        newMapInfo box info =
            -- If route has changed, make sure mapinfo is up to date.
            { info
                | box = box
                , points = model.trackPoints
                , centreLat = Length.inMeters <| BoundingBox3d.midY box
                , centreLon = Length.inMeters <| BoundingBox3d.midX box
                , mapZoom = 12.0 -- TODO: Adjust for bounding box dimensions.
            }
    in
    { model
        | zoomLevelOverview = zoomLevel
        , zoomLevelFirstPerson = zoomLevel
        , zoomLevelThirdPerson = zoomLevel
        , zoomLevelProfile = zoomLevel
        , zoomLevelPlan = zoomLevel
        , azimuth = Angle.degrees 0.0
        , elevation = Angle.degrees 30.0
        , currentNode = 0
        , markedNode = Nothing
        , flythrough = Nothing
        , undoStack = []
        , redoStack = []
        , mapInfo = Maybe.map (newMapInfo model.trackPointBox) model.mapInfo
    }


deriveProblems : Model -> Model
deriveProblems model =
    let
        suddenGradientChanges =
            List.filterMap identity <|
                -- Filters out Nothings (nice)
                List.map2 compareGradients
                    model.roads
                    (List.drop 1 model.roads)

        suddenBearingChanges =
            List.filterMap identity <|
                -- Filters out Nothings (nice)
                List.map2 compareBearings
                    model.roads
                    (List.drop 1 model.roads)

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

        loopy =
            let
                maybeGap =
                    trackPointGap
                        (List.head model.trackPoints)
                        (List.head <| List.reverse model.trackPoints)
            in
            case maybeGap of
                Just ( gap, heightDiff ) ->
                    if gap < 1.0 && heightDiff < 1.0 then
                        IsALoop

                    else if gap < 1000.0 then
                        AlmostLoop gap

                    else
                        NotALoop gap

                _ ->
                    NotALoop 0.0
    in
    { model
        | abruptGradientChanges = suddenGradientChanges
        , abruptBearingChanges = suddenBearingChanges
        , zeroLengths = zeroLengths
        , smoothingEndIndex = Nothing
        , loopiness = loopy
    }


trackPointGap : Maybe TrackPoint -> Maybe TrackPoint -> Maybe ( Float, Float )
trackPointGap t1 t2 =
    case ( t1, t2 ) of
        ( Just tp1, Just tp2 ) ->
            Just
                ( Spherical.range
                    ( degrees tp1.lon, degrees tp1.lat )
                    ( degrees tp2.lon, degrees tp2.lat )
                , abs (tp1.ele - tp2.ele)
                )

        _ ->
            Nothing


deriveStaticVisualEntities : Model -> Model
deriveStaticVisualEntities model =
    -- These need building only when a file is loaded, or a fix is applied.
    let
        newMapInfo =
            Maybe.map updateMapInfo model.mapInfo

        updateMapInfo info =
            { info | points = model.trackPoints }

        marker =
            Maybe.withDefault model.currentNode model.markedNode

        context =
            { displayOptions = model.displayOptions
            , currentNode = Array.get model.currentNode model.nodeArray
            , markedNode =
                case model.markedNode of
                    Just n ->
                        Array.get n model.nodeArray

                    Nothing ->
                        Nothing
            , nodeBox = model.nodeBox
            , viewingMode = model.viewingMode
            , smoothedBend = model.smoothedRoads
            , horizontalNudge = model.nudgeValue
            , verticalNudge = model.verticalNudgeValue
            , nudgedRoads = model.nudgedNodeRoads
            , nudgedRegionStart = Just model.nudgedRegionStart
            }
    in
    { model
        | staticVisualEntities = makeStatic3DEntities context model.roads
        , staticProfileEntities = makeStaticProfileEntities context model.roads
        , mapVisualEntities = makeMapEntities context model.roads
        , mapInfo = newMapInfo
    }


deriveTerrain : Model -> Model
deriveTerrain model =
    -- Terrain building is O(n^2). Not to be undertaken lightly.
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        context =
            { displayOptions = model.displayOptions
            , currentNode = Array.get model.currentNode model.nodeArray
            , markedNode =
                case model.markedNode of
                    Just n ->
                        Array.get n model.nodeArray

                    Nothing ->
                        Nothing
            , nodeBox = model.nodeBox
            , viewingMode = model.viewingMode
            , smoothedBend = model.smoothedRoads
            , horizontalNudge = model.nudgeValue
            , verticalNudge = model.verticalNudgeValue
            , nudgedRoads = model.nudgedNodeRoads
            , nudgedRegionStart = Just model.nudgedRegionStart
            }
    in
    { model
        | terrainEntities = makeTerrain context model.roads
    }


deriveVaryingVisualEntities : Model -> Model
deriveVaryingVisualEntities model =
    -- Refers to the current and marked nodes.
    -- These need building each time the user changes current or marked nodes.
    let
        currentRoad =
            lookupRoad model model.currentNode

        marker =
            Maybe.withDefault model.currentNode model.markedNode

        markedRoad =
            lookupRoad model marker

        context =
            { displayOptions = model.displayOptions
            , currentNode = Array.get model.currentNode model.nodeArray
            , markedNode =
                case model.markedNode of
                    Just n ->
                        Array.get n model.nodeArray

                    Nothing ->
                        Nothing
            , nodeBox = model.nodeBox
            , viewingMode = model.viewingMode
            , smoothedBend = model.smoothedRoads
            , nudgedRoads = model.nudgedNodeRoads
            , nudgedRegionStart = Just model.nudgedRegionStart
            , horizontalNudge = model.nudgeValue
            , verticalNudge = model.verticalNudgeValue
            }

        profileContext =
            { context
                | currentNode =
                    if model.currentNode < Array.length model.roadArray then
                        Maybe.map .profileStartsAt <| Array.get model.currentNode model.roadArray

                    else
                        Maybe.map .profileEndsAt <| Array.get (Array.length model.roadArray - 1) model.roadArray
                , markedNode =
                    case model.markedNode of
                        Just n ->
                            if n < Array.length model.roadArray then
                                Maybe.map .profileStartsAt <| Array.get n model.roadArray

                            else
                                Maybe.map .profileEndsAt <| Array.get (Array.length model.roadArray - 1) model.roadArray

                        Nothing ->
                            Nothing
            }
    in
    { model
        | varyingVisualEntities =
            makeVaryingVisualEntities
                context
                model.roadArray
        , varyingProfileEntities =
            makeVaryingProfileEntities
                profileContext
                model.roads
    }


viewGenericNew : Model -> Browser.Document Msg
viewGenericNew model =
    { title = "GPX viewer"
    , body =
        [ layout
            [ width fill
            , padding 20
            , spacing 20
            , Font.size 16
            ]
          <|
            column
                []
                [ row [ centerX, spaceEvenly, spacing 20, padding 10 ]
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
                    , text <| showDecimal6 model.deltaY
                    ]
                , row [ alignLeft, moveRight 100 ]
                    [ viewModeChoices model
                    ]
                , case ( model.gpx, model.trackPoints ) of
                    ( _, tp1 :: _ ) ->
                        -- Must have at least one track point.
                        row [ alignLeft, alignTop ]
                            [ view3D model.nodeBox model
                            , accordionView (updatedAccordion model) AccordionMessage
                            ]

                    ( Just _, [] ) ->
                        viewInputError model

                    _ ->
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
            [ Input.optionWith ThirdPersonView <| radioButton First "Third person"
            , Input.optionWith FirstPersonView <| radioButton Mid "First person"
            , Input.optionWith ProfileView <| radioButton Mid "Elevation"
            , Input.optionWith PlanView <| radioButton Mid "Plan"
            , Input.optionWith MapView <| radioButton Mid "Map"
            , Input.optionWith AboutView <| radioButton Last "About"
            ]
        }


view3D : BoundingBox3d Length.Meters LocalCoords -> Model -> Element Msg
view3D scale model =
    -- The only differences are (should be) which zoom slider and which projection to use.
    case model.viewingMode of
        FirstPersonView ->
            viewFirstPerson model

        ThirdPersonView ->
            viewThirdPerson model

        ProfileView ->
            viewProfileView model

        AboutView ->
            viewAboutText

        InputErrorView ->
            viewInputError model

        PlanView ->
            viewPlanView model

        MapView ->
            -- We merely create the placeholder, the work is done by messages through the map port.
            el
                [ width (px 880)
                , height (px 650)
                , alignLeft
                , alignTop
                , htmlAttribute (id "map")
                ]
                none


viewInputError : Model -> Element Msg
viewInputError model =
    row
        [ centerX
        , Background.color <| rgb255 220 220 200
        , padding 20
        , Border.width 2
        , Border.color <| rgb255 50 50 50
        , clipY
        , scrollbarY
        ]
        [ el
            [ width <| px 800
            , height <| maximum 600 fill
            ]
          <|
            if List.length model.trackPoints == 0 then
                case model.gpx of
                    Just content ->
                        column [ spacing 10, padding 10 ]
                            [ text "I wanted to see lat, lon and ele data."
                            , text "This is what I found instead."
                            , text <| content
                            ]

                    Nothing ->
                        text "I seem to have no filename."

            else
                text "Nothing to see here."
        ]


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
    column [ spacing 10, padding 20, centerX ]
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
    column [ spacing 10, padding 20, centerX ]
        [ bearingChangeThresholdSlider model
        , wrappedRow [ width <| px 300 ] <|
            List.map linkButton model.abruptBearingChanges
        ]


buttonHighlightCurrent : Int -> Model -> List (Attribute msg)
buttonHighlightCurrent index model =
    if index == model.currentNode then
        [ Background.color <| rgb255 114 159 207, alignRight ]

    else
        [ Background.color <| rgb255 0xFF 0xFF 0xFF, alignRight ]


viewOptions : Model -> Element Msg
viewOptions model =
    column
        [ paddingEach { left = 10, right = 10, top = 10, bottom = 0 }
        , alignTop
        , spacing 10
        , centerX
        ]
        [ if model.terrainEntities == [] then
            button prettyButtonStyles
                { onPress = Just MakeTerrain
                , label = text """Build terrain
(I understand this may take several
minutes and will be lost if I make changes)"""
                }

          else
            button prettyButtonStyles
                { onPress = Just ClearTerrain
                , label = text "Remove the terrain."
                }
        , paragraph
            [ padding 10
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
        , Input.checkbox [ Font.size 18 ]
            { onChange = ToggleCentreLine
            , icon = checkboxIcon
            , checked = model.displayOptions.centreLine
            , label = Input.labelRight [] (text "Centre line")
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
                , Input.optionWith PastelCurtain <| radioButton Mid "Pastel"
                , Input.optionWith RainbowCurtain <| radioButton Last "Rainbow"
                ]
            }
        ]


gradientChangeThresholdSlider : Model -> Element Msg
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
        { onChange = round >> SetMaxTurnPerSegment
        , label =
            Input.labelBelow [] <|
                text <|
                    "Road segments = "
                        ++ String.fromInt model.numLineSegmentsForBend
        , min = 2.0
        , max = 10.0
        , step = Just 1.0
        , value = toFloat model.numLineSegmentsForBend
        , thumb = Input.defaultThumb
        }


viewPointCloud : BoundingBox3d Length.Meters LocalCoords -> Model -> Element Msg
viewPointCloud scale model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0.0 0.0 0.0
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters <| distanceFromZoom model.zoomLevelOverview
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    row [ alignTop, width fill ]
        [ zoomSlider model.zoomLevelOverview ZoomLevelOverview
        , el
            withMouseCapture
          <|
            html <|
                Scene3d.sunny
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.backgroundColor Color.lightBlue
                    , clipDepth = Length.meters 1.0 -- * scale.metresToClipSpace)
                    , entities =
                        model.varyingVisualEntities
                            ++ model.staticVisualEntities
                            ++ model.terrainEntities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        ]


updatedAccordion model =
    -- We have to reapply the accordion update functions with the current model,
    let
        blendAccordionStatus currentAccordionState refreshedContent =
            { currentAccordionState | content = refreshedContent.content }
    in
    List.map2
        blendAccordionStatus
        model.accordion
        (genericAccordion model)


overviewSummary model =
    case model.summary of
        Just summary ->
            row [ padding 20, centerX ]
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


viewLoopTools : Model -> Element Msg
viewLoopTools model =
    let
        loopButton =
            button
                prettyButtonStyles
                { onPress = Just CloseTheLoop
                , label =
                    text <|
                        "Make the track into a loop"
                }

        reverseButton =
            button
                prettyButtonStyles
                { onPress = Just ReverseTrack
                , label =
                    text <|
                        "Reverse the track"
                }

        changeStartButton c =
            button
                prettyButtonStyles
                { onPress = Just (ChangeLoopStart c)
                , label =
                    text <|
                        "Move start/finish to current point"
                }
    in
    el [ spacing 10, padding 20, centerX ] <|
        case model.loopiness of
            IsALoop ->
                column [ spacing 10 ]
                    [ text "This track is a loop."
                    , changeStartButton model.currentNode
                    , reverseButton
                    , undoButton model
                    ]

            AlmostLoop gap ->
                column [ spacing 10 ]
                    [ text <| "This track is " ++ showDecimal2 gap ++ " away from a loop"
                    , loopButton
                    , reverseButton
                    , undoButton model
                    ]

            NotALoop gap ->
                column [ spacing 10 ]
                    [ text <| "This track is " ++ showDecimal2 gap ++ " away from a loop"
                    , loopButton
                    , reverseButton
                    , undoButton model
                    ]


positionControls model =
    let
        forwards =
            FeatherIcons.skipForward
                |> FeatherIcons.toHtml []

        backwards =
            FeatherIcons.skipBack
                |> FeatherIcons.toHtml []
    in
    row
        [ spacing 5
        , padding 5
        , centerX
        , centerY
        ]
        [ positionSlider model
        , button
            prettyButtonStyles
            { onPress = Just PositionBackOne
            , label = html backwards
            }
        , button
            prettyButtonStyles
            { onPress = Just PositionForwardOne
            , label = html forwards
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
        , value = toFloat model.currentNode
        , thumb = Input.defaultThumb
        }


viewFirstPerson model =
    let
        nodeNum =
            min model.currentNode (List.length model.nodes - 2)

        node =
            lookupRoad model nodeNum
    in
    case node of
        Just realNode ->
            row [ alignTop ]
                [ column
                    [ alignTop
                    ]
                    [ viewRoadSegment model realNode
                    , positionControls model
                    ]
                ]

        Nothing ->
            text "Something wrong here."


flythroughControls : Model -> Element Msg
flythroughControls model =
    let
        reset =
            FeatherIcons.rewind
                |> FeatherIcons.toHtml []

        play =
            FeatherIcons.play
                |> FeatherIcons.toHtml []

        pause =
            FeatherIcons.pause
                |> FeatherIcons.toHtml []

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
                , label = html reset
                }

        playButton =
            button
                prettyButtonStyles
                { onPress = Just (RunFlythrough True)
                , label = html play
                }

        pauseButton =
            button
                prettyButtonStyles
                { onPress = Just (RunFlythrough False)
                , label = html pause
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
    in
    row [ padding 10, spacing 10, centerX ]
        [ resetButton
        , playPauseButton
        , flythroughSpeedSlider
        ]


viewRoadSegment : Model -> DrawingRoad -> Element Msg
viewRoadSegment model road =
    let
        eyePoint =
            case model.flythrough of
                Nothing ->
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 eyeHeight)
                        road.startsAt.location

                Just flying ->
                    flying.cameraPosition

        cameraViewpoint =
            case model.flythrough of
                Nothing ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint =
                            Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 eyeHeight)
                                road.endsAt.location
                        , upDirection = Direction3d.positiveZ
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }

        camera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees <| 120.0 / model.zoomLevelFirstPerson
                }
    in
    row []
        [ zoomSlider model.zoomLevelFirstPerson ZoomLevelFirstPerson
        , el
            withMouseCapture
          <|
            html <|
                Scene3d.sunny
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.backgroundColor Color.lightBlue
                    , clipDepth = Length.meters 1.0
                    , entities =
                        model.varyingVisualEntities
                            ++ model.staticVisualEntities
                            ++ model.terrainEntities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        ]


viewThirdPerson : Model -> Element Msg
viewThirdPerson model =
    -- Let's the user spin around and zoom in on selected road point.
    case Array.get model.currentNode model.nodeArray of
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
                ]


viewPlanView : Model -> Element Msg
viewPlanView model =
    case Array.get model.currentNode model.nodeArray of
        Nothing ->
            none

        Just node ->
            row [ alignTop ]
                [ column
                    [ alignTop
                    ]
                    [ viewCurrentNodePlanView model node
                    , positionControls model
                    ]
                ]


viewProfileView : Model -> Element Msg
viewProfileView model =
    -- Let's the user spin around and zoom in on selected road point.
    let
        getNode =
            -- Sadly, only roads have the profile locations, so we need to finesse the last node.
            if model.currentNode < Array.length model.roadArray then
                Maybe.map .profileStartsAt <|
                    Array.get model.currentNode model.roadArray

            else
                Maybe.map .profileEndsAt <|
                    Array.get (Array.length model.roadArray - 1) model.roadArray
    in
    case getNode of
        Nothing ->
            none

        Just node ->
            row [ alignTop ]
                [ column
                    [ alignTop
                    ]
                    [ viewRouteProfile model node
                    , positionControls model
                    ]
                ]


viewBendFixerPane : Model -> Element Msg
viewBendFixerPane model =
    let
        fixBendButton smooth =
            button
                prettyButtonStyles
                { onPress = Just SmoothBend
                , label =
                    text <|
                        "Smooth between markers\nRadius "
                            ++ showDecimal2 smooth.radius
                }
    in
    column [ spacing 10, padding 10, alignTop, centerX ]
        [ markerButton model
        , case model.smoothedBend of
            Just smooth ->
                column [ spacing 10, padding 10, alignTop ]
                    [ fixBendButton smooth
                    , bendSmoothnessSlider model
                    ]

            Nothing ->
                text "Sorry, failed to find a nice bend."
        , undoButton model
        , viewBearingChanges model
        ]


viewStraightenTools : Model -> Element Msg
viewStraightenTools model =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode
    in
    column [ spacing 10, padding 10, alignTop, centerX ]
        [ markerButton model
        , if model.currentNode /= marker then
            straightenButton

          else
            none
        , undoButton model
        ]


viewNudgeTools : Model -> Element Msg
viewNudgeTools model =
    --2020-12-08 Adding tools to Nudge node, split straight, straighten straight.
    column [ padding 5, spacing 10, centerX ]
        [ markerButton model
        , row [ spacing 10, centerX ]
            [ verticalNudgeSlider model.verticalNudgeValue
            , horizontalNudgeSlider model.nudgeValue
            ]
        , nudgeButton model.nudgeValue model.verticalNudgeValue
        , undoButton model
        ]


markerButton model =
    let
        forward =
            FeatherIcons.skipForward
                |> FeatherIcons.toHtml []

        back =
            FeatherIcons.skipBack
                |> FeatherIcons.toHtml []

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
                    , label = html back
                    }
                , makeButton "Clear marker"
                , button
                    prettyButtonStyles
                    { onPress = Just MarkerForwardOne
                    , label = html forward
                    }
                ]

            Nothing ->
                [ makeButton "Drop marker to select a range" ]


undoButton model =
    row [ spacing 5, Border.width 1, Border.rounded 5 ]
        [ button
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
        , button
            prettyButtonStyles
            { onPress =
                case model.redoStack of
                    [] ->
                        Nothing

                    _ ->
                        Just Redo
            , label =
                case model.redoStack of
                    u :: _ ->
                        text <| "Redo " ++ u.label

                    _ ->
                        text "Nothing to redo"
            }
        ]


viewGradientFixerPane : Model -> Element Msg
viewGradientFixerPane model =
    let
        markedNode =
            Maybe.withDefault model.currentNode model.markedNode

        start =
            min model.currentNode markedNode

        finish =
            max model.currentNode markedNode

        avg =
            averageGradient model start finish

        gradientSmoothControls =
            case avg of
                Just gradient ->
                    column [ Border.width 1, spacing 5, padding 5 ]
                        [ button
                            prettyButtonStyles
                            { onPress = Just <| SmoothGradient gradient
                            , label =
                                text <|
                                    "Smooth between markers\nAverage gradient "
                                        ++ showDecimal2 gradient
                            }
                        , smoothnessSlider model
                        ]

                _ ->
                    none
    in
    column [ spacing 10, centerX ] <|
        [ markerButton model
        , gradientSmoothControls
        , undoButton model
        , viewGradientChanges model
        ]


viewTrackPointTools model =
    column [ spacing 10, centerX ] <|
        [ insertNodeOptionsBox model.currentNode
        , markerButton model
        , splitSegmentOptions model.currentNode model.maxSegmentSplitSize
        , deleteNodeButton model.currentNode
        , undoButton model
        ]


insertNodeOptionsBox c =
    row [ spacing 10 ]
        [ button
            prettyButtonStyles
            { onPress = Just (InsertBeforeOrAfter c InsertNodeAfter)
            , label = text "Put two trackpoints in\nplace of this one"
            }

        --, button
        --    prettyButtonStyles
        --    { onPress = Just (InsertBeforeOrAfter c InsertNodeBefore)
        --    , label = text "Insert a node\nbefore this one"
        --    }
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


lookupRoad : Model -> Int -> Maybe DrawingRoad
lookupRoad model idx =
    Array.get idx model.roadArray


viewCurrentNode : Model -> DrawingNode -> Element Msg
viewCurrentNode model node =
    let
        focus =
            case model.flythrough of
                Just fly ->
                    fly.cameraPosition

                Nothing ->
                    node.location

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = focus
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance =
                            Length.meters <|
                                distanceFromZoom model.zoomLevelThirdPerson
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
                    , clipDepth = Length.meters 1.0
                    , entities =
                        model.varyingVisualEntities
                            ++ model.staticVisualEntities
                            ++ model.terrainEntities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        ]


viewCurrentNodePlanView : Model -> DrawingNode -> Element Msg
viewCurrentNodePlanView model node =
    let
        focus =
            Point3d.projectOnto Plane3d.xy
                node.location

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                node.location

        camera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = focus
                        , eyePoint = eyePoint
                        , upDirection = positiveY
                        }
                , viewportHeight = Length.meters <| 2.0 * 10.0 ^ (5.0 - model.zoomLevelPlan)
                }
    in
    row []
        [ zoomSlider model.zoomLevelPlan ZoomLevelPlan
        , el withMouseCapture
          <|
            html <|
                Scene3d.sunny
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.backgroundColor Color.darkGreen
                    , clipDepth = Length.meters 1.0
                    , entities = model.varyingVisualEntities ++ model.staticVisualEntities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        ]


viewCentredPlanViewForMap : BoundingBox3d Length.Meters LocalCoords -> Model -> Element Msg
viewCentredPlanViewForMap box model =
    -- Challenge is to optimize zoom level to fit map.
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box

        focus =
            BoundingBox3d.centerPoint box

        ( xSize, ySize, zSize ) =
            BoundingBox3d.dimensions box

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                Point3d.origin

        viewPort =
            Length.meters <|
                max
                    (Length.inMeters ySize)
                    (Length.inMeters xSize * 500.0 / 800.0)

        camera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = focus
                        , eyePoint = eyePoint
                        , upDirection = positiveY
                        }
                , viewportHeight = viewPort
                }
    in
    el
        [ width (px 800), height (px 500), alignLeft, alignTop ]
    <|
        html <|
            Scene3d.sunny
                { camera = camera
                , dimensions = ( Pixels.int 800, Pixels.int 500 )
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 1.0
                , entities = model.mapVisualEntities
                , upDirection = positiveZ
                , sunlightDirection = negativeZ
                , shadows = True
                }


viewRouteProfile : Model -> DrawingNode -> Element Msg
viewRouteProfile model node =
    let
        focus =
            Point3d.projectOnto
                Plane3d.yz
                node.location

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 100.0 0.0 0.0)
                node.location

        camera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = focus
                        , eyePoint = eyePoint
                        , upDirection = positiveZ
                        }
                , viewportHeight = Length.meters <| 1.0 * 10.0 ^ (4.0 - model.zoomLevelProfile)
                }
    in
    row []
        [ zoomSlider model.zoomLevelProfile ZoomLevelProfile
        , el
          withMouseCapture
          <|
            html <|
                Scene3d.sunny
                    { camera = camera
                    , dimensions = ( Pixels.int 800, Pixels.int 500 )
                    , background = Scene3d.backgroundColor Color.lightCharcoal
                    , clipDepth = Length.meters 1.0
                    , entities = model.varyingProfileEntities ++ model.staticProfileEntities
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = True
                    }
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ messageReceiver MapMessage
        , mapStopped MapRemoved
        , Time.every 10 Tick
        ]
