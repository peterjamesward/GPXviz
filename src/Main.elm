module Main exposing (main)

import About exposing (viewAboutText)
import Accordion exposing (..)
import Angle exposing (Angle, inDegrees)
import Area
import Array exposing (Array)
import AutoFix exposing (autoFix)
import BendSmoother exposing (SmoothedBend, lookForSmoothBendOption)
import BoundingBox3d exposing (BoundingBox3d)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Camera3d exposing (Camera3d)
import Color
import ColourPalette exposing (..)
import Delay exposing (TimeUnit(..), after)
import Direction3d exposing (negativeZ, positiveY, positiveZ)
import DisplayOptions exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button, text)
import FeatherIcons
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Filters exposing (applyWeightedAverageFilter)
import Flythrough exposing (Flythrough, eyeHeight, flythrough)
import GeoCodeDecoders exposing (IpInfo)
import Geometry101
import Html.Attributes exposing (id)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..), Event)
import Http
import Json.Decode as E exposing (..)
import Length exposing (inMeters, meters)
import List exposing (drop, take)
import Loop exposing (..)
import MapController exposing (MapInfo, MapState(..), mapPort, mapStopped, messageReceiver, msgDecoder)
import MapboxStuff exposing (metresPerPixel, zoomLevelFromBoundingBox)
import Msg exposing (..)
import MyIP exposing (requestIpInformation)
import NodesAndRoads exposing (..)
import Nudge exposing (nudgeTrackPoint)
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, distanceFromAxis, xCoordinate, yCoordinate, zCoordinate)
import Rectangle2d
import Scene3d exposing (Entity)
import ScenePainter exposing (render3dScene)
import Spherical exposing (metresPerDegree)
import StravaAuth exposing (getStravaToken, stravaButton)
import StravaDataLoad exposing (requestStravaRoute, requestStravaRouteHeader, requestStravaSegment, requestStravaSegmentStreams, stravaApiRoot, stravaProcessRoute, stravaProcessSegment, stravaRouteName)
import StravaPasteStreams exposing (pasteStreams)
import StravaTypes exposing (StravaRoute, StravaRouteStatus(..), StravaSegment, StravaSegmentStatus(..))
import Task
import Terrain exposing (makeTerrain)
import Time
import TrackPoint exposing (..)
import Triangle3d
import Url exposing (Url)
import Url.Builder as Builder
import Utils exposing (..)
import Vector2d
import Vector3d
import ViewElements exposing (..)
import ViewTypes exposing (..)
import Viewpoint3d
import VisualEntities exposing (..)
import WriteGPX exposing (writeGPX)


main : Program (Maybe (List Int)) Model Msg
main =
    -- This is the 'main' from OAuth example/
    application
        { init =
            Maybe.map StravaAuth.convertBytes >> init
        , update =
            update
        , subscriptions = subscriptions
        , onUrlRequest =
            always (OAuthMessage NoOp)
        , onUrlChange =
            always (OAuthMessage NoOp)
        , view =
            view
        }


type LoadingStep
    = LoadNone
    | LoadGetFileDetails
    | LoadReadFile
    | LoadParseFile
    | LoadDeriveNodes
    | LoadDeriveVisuals
    | LoadFinishUp


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


type DragAction
    = DragNone
    | DragRotate
    | DragPan
    | DragProfile
    | DragPlan


type GpxSource
    = GpxNone
    | GpxLocalFile
    | GpxStrava
    | GpxKomoot


type alias Model =
    { loadingStep : LoadingStep
    , gpx : Maybe String
    , gpxSource : GpxSource
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
    , dragAction : DragAction
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
    , changeCounter : Int
    , smoothingEndIndex : Maybe Int
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , smoothedBend : Maybe SmoothedBend -- computed track points
    , bendTrackPointSpacing : Float -- How far apart TPs are when bend smoothed.
    , bumpinessFactor : Float -- 0.0 => average gradient, 1 => original gradients
    , flythroughSpeed : Float
    , flythrough : Maybe Flythrough
    , loopiness : Loopiness
    , nudgeValue : Float
    , nudgedNodeRoads : List DrawingRoad
    , nudgedRegionStart : Int -- so we can correlate nudgedNodeRoads in profile view (ugh).
    , verticalNudgeValue : Float
    , toolsAccordion : List (AccordionEntry Msg)
    , infoAccordion : List (AccordionEntry Msg)
    , maxSegmentSplitSize : Float
    , mapInfo : Maybe MapController.MapInfo
    , currentSceneCamera : Maybe (Camera3d.Camera3d Length.Meters LocalCoords)
    , clickData : ( Float, Float )
    , mouseDownTime : Time.Posix -- seems needed to distinguish click from any other mouse-up event.
    , cameraFocusThirdPerson : Point3d Length.Meters LocalCoords
    , cameraFocusProfileNode : Int
    , cameraFocusPlan : Point3d Length.Meters LocalCoords
    , metricFilteredNodes : List Int -- candidate track points to be removed to simplify a big track.
    , externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , stravaRoute : StravaRouteStatus
    , stravaAuthentication : O.Model
    , mapNodesDraggable : Bool
    , lastHttpError : Maybe Http.Error
    , ipInfo : Maybe IpInfo
    , filterModes : ( Bool, Bool ) -- (XY, Z)
    }


init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    -- We stitch in the OAuth init stuff somehow here.
    let
        ( authData, authCmd ) =
            StravaAuth.init mflags origin navigationKey wrapAuthMessage
    in
    ( { loadingStep = LoadNone
      , gpx = Nothing
      , gpxSource = GpxNone
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
      , dragAction = DragNone
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
      , zoomLevelFirstPerson = 12.0
      , zoomLevelThirdPerson = 12.0
      , zoomLevelProfile = 12.0
      , zoomLevelPlan = 12.0
      , displayOptions = defaultDisplayOptions
      , abruptGradientChanges = []
      , abruptBearingChanges = []
      , zeroLengths = []
      , gradientChangeThreshold = 10.0 -- Note, this is not an angle, it's a percentage (tangent).
      , bearingChangeThreshold = 90
      , changeCounter = 0
      , smoothingEndIndex = Nothing
      , undoStack = []
      , redoStack = []
      , smoothedBend = Nothing
      , bendTrackPointSpacing = 3
      , bumpinessFactor = 0.0
      , flythrough = Nothing
      , flythroughSpeed = 1.0
      , loopiness = NotALoop 0.0
      , nudgeValue = 0.0
      , nudgedNodeRoads = []
      , nudgedRegionStart = 0
      , verticalNudgeValue = 0.0
      , toolsAccordion = []
      , infoAccordion = []
      , maxSegmentSplitSize = 30.0 -- When we split a segment, how close should the track points be.
      , mapInfo = Nothing
      , currentSceneCamera = Nothing
      , clickData = ( 0.0, 0.0 )
      , mouseDownTime = Time.millisToPosix 0
      , cameraFocusThirdPerson = Point3d.origin
      , cameraFocusProfileNode = 0
      , cameraFocusPlan = Point3d.origin
      , metricFilteredNodes = []
      , externalSegmentId = ""
      , externalSegment = SegmentNone
      , externalRouteId = ""
      , stravaRoute = StravaRouteNone
      , stravaAuthentication = authData
      , mapNodesDraggable = False
      , lastHttpError = Nothing
      , ipInfo = Nothing
      , filterModes = ( True, True )
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , authCmd
        ]
    )


toolsAccordion model =
    [ { label = "Loop maker"
      , state = Contracted
      , content = viewLoopTools model
      }
    , { label = "Smooth bend"
      , state = Contracted
      , content = viewBendFixerPane model
      }
    , { label = "Smooth gradient"
      , state = Contracted
      , content = viewGradientFixerPane model
      }
    , { label = "Nudge "
      , state = Contracted
      , content = viewNudgeTools model
      }
    , { label = "Straighten"
      , state = Contracted
      , content = viewStraightenTools model
      }
    , { label = "Trackpoints"
      , state = Contracted
      , content = viewTrackPointTools model
      }
    , { label = "Fly-through"
      , state = Contracted
      , content = flythroughControls model
      }
    , { label = "Strava"
      , state = Contracted
      , content = viewStravaDataAccessTab model
      }
    , { label = "Filters"
      , state = Contracted
      , content = viewFilterControls model
      }
    ]


infoAccordion model =
    [ { label = "Summary"
      , state = Expanded
      , content = overviewSummary model
      }
    , { label = "Road segment data"
      , state = Contracted
      , content = summaryData (lookupRoad model model.currentNode)
      }
    , { label = "Visual styles"
      , state = Contracted
      , content = viewOptions model
      }
    , { label = "Gradient problems"
      , state = Contracted
      , content = viewGradientChanges model
      }
    , { label = "Bend problems"
      , state = Contracted
      , content = viewBearingChanges model
      }
    , { label = "Map options"
      , state = Contracted
      , content = viewMapOptions model
      }
    ]


initialiseAccordion : Model -> Model
initialiseAccordion model =
    let
        _ = Debug.log "Resetting tools" ""
    in
    { model
        | toolsAccordion = toolsAccordion model
        , infoAccordion = infoAccordion model
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
        , changeCounter = model.changeCounter + 1
    }


clearTheModel model =
    let
        _ = Debug.log "Clearing the model" 0
    in
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
        , changeCounter = 0
        , smoothingEndIndex = Nothing
        , undoStack = []
        , redoStack = []
        , smoothedBend = Nothing
        , flythrough = Nothing
        , loopiness = NotALoop 0.0
        , nudgedNodeRoads = []
    }


locallyHandleMapMessage : Model -> E.Value -> ( Model, Cmd Msg )
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
                    ( model
                        |> makeNearestNodeCurrent lon1 lat1
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
                    , updateMapVaryingElements model
                    )

                _ ->
                    ( model, Cmd.none )

        Ok "drag" ->
            model |> draggedOnMap json |> trackHasChanged

        _ ->
            ( model, Cmd.none )


draggedOnMap : E.Value -> Model -> Model
draggedOnMap json model =
    -- Map has told us the old and new coordinates of a trackpoint.
    let
        lon1 =
            E.decodeValue (at [ "start", "lng" ] float) json

        lat1 =
            E.decodeValue (at [ "start", "lat" ] float) json

        lon2 =
            E.decodeValue (at [ "end", "lng" ] float) json

        lat2 =
            E.decodeValue (at [ "end", "lat" ] float) json

        newModel tp endLon endLat =
            addToUndoStack "Drag track point" model
                |> (\m ->
                        { m
                            | trackPoints =
                                List.take tp.idx model.trackPoints
                                    ++ [ { tp | lon = endLon, lat = endLat } ]
                                    ++ List.drop (tp.idx + 1) model.trackPoints
                            , currentNode = tp.idx
                        }
                   )
    in
    case ( ( lon1, lat1 ), ( lon2, lat2 ) ) of
        ( ( Ok startLon, Ok startLat ), ( Ok endLon, Ok endLat ) ) ->
            let
                maybetp =
                    findTrackPoint startLon startLat model.trackPoints
            in
            case maybetp of
                Just tp ->
                    newModel tp endLon endLat

                Nothing ->
                    model

        _ ->
            model


makeNearestNodeCurrent : Float -> Float -> Model -> Model
makeNearestNodeCurrent lon lat model =
    -- Searching like this may be too slow. Wait and see.
    -- Speed up 1 - do approximate filter then exact on subset.
    -- Speed up 2 - use quadtree structure (!)
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
            { model
                | currentNode = n.index
                , cameraFocusProfileNode = n.index
            }

        _ ->
            model


detectHit : Model -> Mouse.Event -> Model
detectHit model event =
    -- Same but in our local coord, not map coords.
    -- Need assistance from elm-3d-scene.
    let
        ( x, y ) =
            event.offsetPos

        screenPoint =
            Point2d.pixels x y

        screenRectangle =
            Rectangle2d.with
                { x1 = Pixels.pixels 0
                , y1 = Pixels.pixels view3dHeight
                , x2 = Pixels.pixels view3dWidth
                , y2 = Pixels.pixels 0
                }

        profileNodes =
            -- Probably something worth having in the model
            List.map .profileStartsAt (List.take 1 model.roads)
                ++ List.map .profileEndsAt model.roads

        nodeList =
            case model.viewingMode of
                ProfileView ->
                    profileNodes

                _ ->
                    model.nodes
    in
    case model.currentSceneCamera of
        Just camera ->
            let
                ray =
                    Camera3d.ray camera screenRectangle screenPoint

                distances =
                    List.map
                        (\node ->
                            ( node.trackPoint.idx
                            , Length.inMeters <| distanceFromAxis ray node.location
                            )
                        )
                        nodeList

                inDistanceOrder =
                    List.sortBy Tuple.second distances
            in
            case List.head inDistanceOrder of
                Just ( idx, _ ) ->
                    { model | currentNode = idx }

                Nothing ->
                    model

        Nothing ->
            model



-- placeholder
{-
   Here's some handy stuff from 'unsoundscapes' via slack.
   (decodeMouseRay camera width height MouseDown)

   decodeMouseRay :
       Camera3d Meters WorldCoordinates
       -> Quantity Float Pixels
       -> Quantity Float Pixels
       -> (Axis3d Meters WorldCoordinates -> msg)
       -> Decoder msg
   decodeMouseRay camera3d width height rayToMsg =
       Json.Decode.map2
           (\x y ->
               rayToMsg
                   (Camera3d.ray
                       camera3d
                       (Rectangle2d.with
                           { x1 = pixels 0
                           , y1 = height
                           , x2 = width
                           , y2 = pixels 0
                           }
                       )
                       (Point2d.pixels x y)
                   )
           )
           (Json.Decode.field "pageX" Json.Decode.float)
           (Json.Decode.field "pageY" Json.Decode.float)

           ...

           MouseDown mouseRay ->
               { model
                   | selection =
                       World.raycast mouseRay world
                           |> Maybe.map (\{ body } -> Body.data body)
               }
-}


findBracketedRange : Model -> ( Int, Int )
findBracketedRange model =
    -- For tools that default to whole track if no range. (Some tools default to single node.)
    case model.markedNode of
        Just marker ->
            ( min model.currentNode marker, max model.currentNode marker )

        Nothing ->
            ( 0, Array.length model.nodeArray - 1 )


commonModelLoader : Model -> String -> GpxSource -> ( Model, Cmd Msg )
commonModelLoader model content source =
    let
        newModel =
            { model | gpxSource = source }
                |> clearTheModel
                |> initialiseAccordion
                |> parseGPXintoModel content
                |> deriveNodesAndRoads
                |> deriveProblems
                |> resetViewSettings
                |> deriveStaticVisualEntities
                |> deriveVaryingVisualEntities
                |> lookForSimplifications
    in
    switchViewMode newModel newModel.viewingMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        options =
            model.displayOptions
    in
    case msg of
        -- Delegate wrapped OAuthmessages. Be bowled over if this works first time. Or fiftieth.
        -- Maybe look after to see if there is yet a token. Easy way to know.
        OAuthMessage authMsg ->
            let
                ( newAuthData, authCmd ) =
                    StravaAuth.update authMsg model.stravaAuthentication

                isToken =
                    getStravaToken newAuthData
            in
            ( { model | stravaAuthentication = newAuthData }
            , Cmd.map OAuthMessage authCmd
            )

        NoOpMsg ->
            ( model, Cmd.none )

        UserChangedFilename txt ->
            ( { model | filename = Just txt }, Cmd.none )

        UserChangedSegmentId url ->
            let
                segmentId =
                    url |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            ( { model
                | externalSegmentId = segmentId
                , externalSegment = SegmentNone
              }
            , Cmd.none
            )

        UserChangedRouteId url ->
            let
                routeId =
                    url |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            ( { model | externalRouteId = routeId }, Cmd.none )

        -- Mainly to suppress the context menu on the pictures!
        MapMessage jsonMsg ->
            case model.mapInfo of
                Just mapInfo ->
                    case MapController.processMapMessage mapInfo jsonMsg of
                        Just ( newInfo, cmd ) ->
                            ( { model | mapInfo = Just newInfo }
                            , cmd
                            )

                        Nothing ->
                            locallyHandleMapMessage model jsonMsg

                Nothing ->
                    ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }
                |> advanceFlythrough newTime
            , Cmd.none
            )

        AccordionMessage entry ->
            ( { model
                | toolsAccordion = accordionToggle model.toolsAccordion entry
                , infoAccordion = accordionToggle model.infoAccordion entry
              }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
            )

        GpxRequested ->
            ( model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            let
                _ = Debug.log "Loading GPX from " file
            in
            ( { model | filename = Just (File.name file) }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxDownloaded response ->
            case response of
                Ok content ->
                    commonModelLoader model content GpxStrava

                Err _ ->
                    ( model, Cmd.none )

        GpxLoaded content ->
            -- TODO: Tidy up the removal of zero length segments,
            -- so as not to repeat ourselves here.
            commonModelLoader model content GpxLocalFile

        UserMovedNodeSlider node ->
            let
                newModel =
                    { model | currentNode = node }
                        |> cancelFlythrough
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
                        |> centreViewOnCurrentNode
                        |> checkSceneCamera
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
                        | currentNode = modBy (Array.length model.nodeArray) (model.currentNode + 1)
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
                        |> cancelFlythrough
                        |> centreViewOnCurrentNode
                        |> checkSceneCamera
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        PositionBackOne ->
            let
                newModel =
                    { model
                        | currentNode = modBy (Array.length model.nodeArray) (model.currentNode - 1)
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
                        |> cancelFlythrough
                        |> centreViewOnCurrentNode
                        |> checkSceneCamera
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
                                (\m -> modBy (Array.length model.nodeArray) (m + 1))
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
                                (\m -> modBy (Array.length model.nodeArray) (m - 1))
                                model.markedNode
                    }
                        |> tryBendSmoother
                        |> deriveVaryingVisualEntities
            in
            ( newModel
            , updateMapVaryingElements newModel
            )

        SetBendTrackPointSpacing turn ->
            let
                newModel =
                    { model
                        | bendTrackPointSpacing = turn
                    }
                        |> tryBendSmoother
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
                        |> checkSceneCamera
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ZoomLevelOverview level ->
            ( { model | zoomLevelOverview = level }
                |> checkSceneCamera
            , Cmd.none
            )

        ZoomLevelFirstPerson level ->
            ( { model | zoomLevelFirstPerson = level }
                |> checkSceneCamera
            , Cmd.none
            )

        ZoomLevelThirdPerson level ->
            ( { model | zoomLevelThirdPerson = level }
                |> checkSceneCamera
            , Cmd.none
            )

        ZoomLevelProfile level ->
            ( { model | zoomLevelProfile = level }
                |> checkSceneCamera
            , Cmd.none
            )

        ZoomLevelPlan level ->
            ( { model | zoomLevelPlan = level }
                |> checkSceneCamera
            , Cmd.none
            )

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                alternate =
                    event.keys.ctrl || event.button == SecondButton
            in
            ( { model
                | orbiting = Just event.offsetPos
                , dragAction =
                    case ( model.viewingMode, alternate ) of
                        ( ThirdPersonView, False ) ->
                            DragPan

                        ( ThirdPersonView, True ) ->
                            DragRotate

                        ( ProfileView, _ ) ->
                            DragProfile

                        ( PlanView, _ ) ->
                            DragPlan

                        _ ->
                            DragNone
                , mouseDownTime = model.time -- to disambinguate click and mouse-up. maybe.
              }
            , Cmd.none
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( model.dragAction, model.orbiting, model.currentSceneCamera ) of
                ( DragRotate, Just ( startX, startY ), Just camera ) ->
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
                        |> checkSceneCamera
                    , Cmd.none
                    )

                ( DragPan, Just ( startX, startY ), Just camera ) ->
                    let
                        currentViewpoint =
                            Camera3d.viewpoint camera

                        xDirection =
                            Viewpoint3d.xDirection currentViewpoint

                        yDirection =
                            Viewpoint3d.yDirection currentViewpoint

                        currentFocus =
                            model.cameraFocusThirdPerson

                        xMovement =
                            Vector3d.withLength
                                (Length.meters <| 0.5 * (startX - dx))
                                xDirection

                        yMovement =
                            Vector3d.withLength
                                (Length.meters <| 0.5 * (dy - startY))
                                yDirection

                        netMovement =
                            Vector3d.plus xMovement yMovement

                        newFocus =
                            Point3d.translateBy netMovement currentFocus
                    in
                    ( { model
                        | cameraFocusThirdPerson = newFocus
                        , orbiting = Just ( dx, dy )
                      }
                        |> checkSceneCamera
                    , Cmd.none
                    )

                ( DragPlan, Just ( startX, startY ), Just camera ) ->
                    let
                        currentViewpoint =
                            Camera3d.viewpoint camera

                        xDirection =
                            Viewpoint3d.xDirection currentViewpoint

                        yDirection =
                            Viewpoint3d.yDirection currentViewpoint

                        currentFocus =
                            model.cameraFocusPlan

                        xMovement =
                            Vector3d.withLength
                                (Length.meters <| 0.5 * (startX - dx))
                                xDirection

                        yMovement =
                            Vector3d.withLength
                                (Length.meters <| 0.5 * (dy - startY))
                                yDirection

                        netMovement =
                            Vector3d.plus xMovement yMovement

                        newFocus =
                            Point3d.translateBy netMovement currentFocus
                    in
                    ( { model
                        | cameraFocusPlan = newFocus
                        , orbiting = Just ( dx, dy )
                      }
                        |> checkSceneCamera
                    , Cmd.none
                    )

                ( DragProfile, Just ( startX, startY ), Just camera ) ->
                    let
                        xMovement =
                            round <| 0.2 * (startX - dx)

                        newFocus =
                            clamp 0 (Array.length model.roadArray - 1) <|
                                model.cameraFocusProfileNode
                                    + xMovement
                    in
                    ( { model
                        | cameraFocusProfileNode = newFocus
                        , orbiting = Just ( dx, dy )
                      }
                        |> checkSceneCamera
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ImageRelease _ ->
            ( { model | orbiting = Nothing, dragAction = DragNone }
            , Cmd.none
            )

        ToggleCones _ ->
            { model
                | displayOptions = { options | roadCones = not options.roadCones }
            }
                |> deriveStaticVisualEntities
                |> synchroniseMap

        ToggleLighting _ ->
            { model
                | displayOptions = { options | withLighting = not options.withLighting }
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
            ( { model | changeCounter = 0 }
            , outputGPX model
            )

        SmoothGradient g ->
            smoothGradient model g
                |> trackHasChanged

        SmoothBend ->
            model
                |> smoothBend
                |> trackHasChanged

        AutoFix nodes ->
            let
                undoMessage =
                    "AutoFix " ++ String.fromInt (List.length nodes) ++ " issues"
            in
            model
                |> addToUndoStack undoMessage
                |> (\m -> { m | trackPoints = autoFix model.trackPoints nodes })
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
                        , changeCounter = model.changeCounter - 1
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
                        , changeCounter = model.changeCounter + 1
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

        DeleteTrackPoints range ->
            deleteTrackPoints range model
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
                factor =
                    -0.001

                newModel =
                    case model.viewingMode of
                        FirstPersonView ->
                            { model
                                | zoomLevelFirstPerson =
                                    clamp 0.0 22.0 <|
                                        model.zoomLevelFirstPerson
                                            + deltaY
                                            * factor
                            }

                        ThirdPersonView ->
                            { model
                                | zoomLevelThirdPerson =
                                    clamp 0.0 22.0 <|
                                        model.zoomLevelThirdPerson
                                            + deltaY
                                            * factor
                            }

                        ProfileView ->
                            { model
                                | zoomLevelProfile =
                                    clamp 0.0 22.0 <|
                                        model.zoomLevelProfile
                                            + deltaY
                                            * factor
                            }

                        PlanView ->
                            { model
                                | zoomLevelPlan =
                                    clamp 0.0 22.0 <|
                                        model.zoomLevelPlan
                                            + deltaY
                                            * factor
                            }

                        _ ->
                            model
            in
            ( newModel |> checkSceneCamera
            , Cmd.none
            )

        MouseClick event ->
            ( if Time.posixToMillis model.time < Time.posixToMillis model.mouseDownTime + 250 then
                detectHit model event
                    |> tryBendSmoother
                    |> deriveVaryingVisualEntities

              else
                model
            , Cmd.none
            )

        MouseDoubleClick event ->
            ( detectHit model event
                |> tryBendSmoother
                |> deriveVaryingVisualEntities
                |> centreViewOnCurrentNode
                |> checkSceneCamera
            , Cmd.none
            )

        SimplifyTrack ->
            let
                ( rangeStart, rangeEnd ) =
                    findBracketedRange model

                undoMessage =
                    "Remove "
                        ++ String.fromInt (List.length nodesToRemove)
                        ++ " track points"

                nodesToRemove =
                    List.filter
                        (\n -> n >= rangeStart && n <= rangeEnd)
                        model.metricFilteredNodes

                replaceTrackPoints old =
                    { old
                        | trackPoints =
                            reindexTrackpoints <|
                                removeByNodeNumbers nodesToRemove model.trackPoints
                    }

                newModel =
                    addToUndoStack undoMessage model
                        |> replaceTrackPoints
                        |> deriveNodesAndRoads
                        |> (case Array.get model.currentNode model.nodeArray of
                                Just node ->
                                    makeNearestNodeCurrent node.trackPoint.lon node.trackPoint.lat

                                Nothing ->
                                    identity
                           )
            in
            trackHasChanged newModel

        ToggleFilterXY setting ->
            let
                ( filterXY, filterZ ) =
                    model.filterModes
            in
            ( { model | filterModes = ( not filterXY, filterZ ) }
            , Cmd.none
            )

        ToggleFilterZ setting ->
            let
                ( filterXY, filterZ ) =
                    model.filterModes
            in
            ( { model | filterModes = ( filterXY, not filterZ ) }
            , Cmd.none
            )

        FilterWeightedAverage ->
            let
                ( rangeStart, rangeEnd ) =
                    --TODO: Apply over bracket.
                    findBracketedRange model

                undoMessage =
                    "Weighted average filter"

                replaceTrackPoints old =
                    { old
                        | trackPoints =
                            applyWeightedAverageFilter
                                ( rangeStart, rangeEnd )
                                model.filterModes
                                model.loopiness
                                model.trackPoints
                    }

                newModel =
                    model
                        |> addToUndoStack undoMessage
                        |> replaceTrackPoints
            in
            trackHasChanged newModel

        LoadExternalSegment ->
            case getStravaToken model.stravaAuthentication of
                Just token ->
                    ( { model | externalSegment = SegmentRequested }
                    , requestStravaSegment HandleSegmentData model.externalSegmentId token
                    )

                Nothing ->
                    ( model, Cmd.none )

        LoadSegmentStreams ->
            case getStravaToken model.stravaAuthentication of
                Just token ->
                    ( model
                    , requestStravaSegmentStreams HandleSegmentStreams model.externalSegmentId token
                    )

                Nothing ->
                    ( model, Cmd.none )

        HandleSegmentData response ->
            ( { model
                | externalSegment = stravaProcessSegment response model.trackPointBox
              }
            , Cmd.none
            )

        HandleSegmentStreams response ->
            case ( response, model.externalSegment ) of
                ( Ok streams, SegmentOk segment ) ->
                    model
                        |> addToUndoStack "Paste Strava segment"
                        |> (\m -> { m | trackPoints = pasteStreams m.trackPoints segment streams })
                        |> trackHasChanged

                ( Err err, _ ) ->
                    ( { model | lastHttpError = Just err }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadExternalRoute ->
            case getStravaToken model.stravaAuthentication of
                Just token ->
                    ( { model | stravaRoute = StravaRouteRequested }
                    , requestStravaRouteHeader HandleRouteData model.externalRouteId token
                    )

                Nothing ->
                    ( model, Cmd.none )

        HandleRouteData response ->
            case getStravaToken model.stravaAuthentication of
                Just token ->
                    let
                        stravaRoute =
                            stravaProcessRoute response
                    in
                    ( { model
                        | stravaRoute = stravaRoute
                        , filename = stravaRouteName stravaRoute
                      }
                    , requestStravaRoute GpxDownloaded model.externalRouteId token
                    )

                Nothing ->
                    ( model, Cmd.none )

        ReceivedIpDetails response ->
            let
                ipInfo =
                    MyIP.processIpInfo response
            in
            ( { model | ipInfo = ipInfo }
            , MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
            )

        IpInfoAcknowledged _ ->
            ( model, Cmd.none )

        ToggleMapNodesDraggable state ->
            ( { model | mapNodesDraggable = state }
            , case model.mapInfo of
                Just info ->
                    MapController.toggleDragging state info

                Nothing ->
                    Cmd.none
            )


trackHasChanged model =
    model
        |> deriveNodesAndRoads
        |> deriveStaticVisualEntities
        |> deriveProblems
        |> lookForSimplifications
        |> clearTerrain
        |> tryBendSmoother
        |> deriveVaryingVisualEntities
        |> synchroniseMap


lookForSimplifications : Model -> Model
lookForSimplifications model =
    let
        _ = Debug.log "Check for simplifications " 0
    in
    { model | metricFilteredNodes = metricFilteredNodes model.nodes }


centreViewOnCurrentNode model =
    case
        ( Array.get model.currentNode model.nodeArray
        , Array.get model.currentNode model.roadArray
        )
    of
        ( Just node, Just road ) ->
            { model
                | cameraFocusThirdPerson = node.location
                , cameraFocusProfileNode = model.currentNode
                , cameraFocusPlan = node.location
            }

        ( Just node, Nothing ) ->
            { model
                | cameraFocusThirdPerson = node.location
                , cameraFocusProfileNode = model.currentNode - 1
                , cameraFocusPlan = node.location
            }

        _ ->
            model


nodeToTrackPoint :
    Point3d Length.Meters GPXCoords
    -> Point3d Length.Meters LocalCoords
    -> TrackPoint
nodeToTrackPoint trackCenter node =
    -- This is the inverse of our map projection ...
    --projectedX lon lat =
    --    lon * metresPerDegree * cos (degrees lat)
    --
    --projectedY lon lat =
    --    lat * metresPerDegree
    let
        ( centerLon, centerLat ) =
            ( Length.inMeters <| xCoordinate trackCenter
            , Length.inMeters <| yCoordinate trackCenter
            )

        ( x, y ) =
            ( Length.inMeters <| xCoordinate node
            , Length.inMeters <| yCoordinate node
            )

        degreesLat =
            centerLat + y / metresPerDegree

        degreesLon =
            centerLon + x / metresPerDegree / cos (degrees degreesLat)
    in
    { lat = degreesLat
    , lon = degreesLon
    , ele = Length.inMeters <| zCoordinate node
    , idx = 0
    }


nodesToTrackPoints :
    Point3d Length.Meters GPXCoords
    -> List (Point3d Length.Meters LocalCoords)
    -> List TrackPoint
nodesToTrackPoints trackCenter nodes =
    List.map (nodeToTrackPoint trackCenter) nodes


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

        centre =
            BoundingBox3d.centerPoint model.trackPointBox
    in
    case ( currentNode, markedNode ) of
        ( Just node1, Just node2 ) ->
            MapController.addMarkersToMap
                ( node1.trackPoint.lon, node1.trackPoint.lat )
                (Just ( node2.trackPoint.lon, node2.trackPoint.lat ))
                (Maybe.withDefault [] <|
                    Maybe.map (.nodes >> nodesToTrackPoints centre) model.smoothedBend
                )
                nudgedTrackPoints

        ( Just node1, Nothing ) ->
            MapController.addMarkersToMap
                ( node1.trackPoint.lon, node1.trackPoint.lat )
                Nothing
                (Maybe.withDefault [] <|
                    Maybe.map (.nodes >> nodesToTrackPoints centre) model.smoothedBend
                )
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
            updatedMapInfo
                { mapState = WaitingForNode
                , box = box
                , points = model.trackPoints
                , nextView = MapView
                , centreLon = Length.inMeters <| BoundingBox3d.midX box
                , centreLat = Length.inMeters <| BoundingBox3d.midY box
                , mapZoom = zoomLevelFromBoundingBox model.trackPointBox
                , current = ( 0.0, 0.0 )
                , marker = Nothing
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
                                , flythrough = Nothing
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
                |> checkSceneCamera
            , MapController.removeMap
            )

        ( _, _ ) ->
            -- Map not involved, happy days.
            ( { model | viewingMode = mode }
                |> deriveVaryingVisualEntities
                |> checkSceneCamera
            , Cmd.none
            )


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



--simulateNodeRangeNudge : Model -> Int -> Int -> Float -> Float -> Model


simulateNodeRangeNudge model node1 nodeN horizontal vertical =
    let
        targetNodes =
            List.drop node1 <| List.take (nodeN + 1) model.nodes

        getBearingForNode : DrawingNode -> Float
        getBearingForNode node =
            let
                precedingRoad =
                    Array.get (node.trackPoint.idx - 1) model.roadArray

                followingRoad =
                    Array.get node.trackPoint.idx model.roadArray

                neighbouringRoads =
                    [ precedingRoad, followingRoad ]

                sumBearings =
                    List.sum <| List.filterMap (Maybe.map .bearing) neighbouringRoads

                numBearings =
                    List.length <| List.filterMap (Maybe.map .bearing) neighbouringRoads
            in
            sumBearings / toFloat numBearings

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
            if model.currentNode == marker then
                -- Apply to whole track
                ( 0, Array.length model.nodeArray - 1 )

            else
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
    model |> addToUndoStack undoMessage |> makeItSo


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


deleteTrackPoints : ( Int, Int ) -> Model -> Model
deleteTrackPoints ( start, finish ) model =
    let
        undoMessage =
            "Delete track points " ++ String.fromInt start ++ "-" ++ String.fromInt finish
    in
    let
        precedingTPs =
            List.take start model.trackPoints

        remainingTPs =
            List.drop (finish + 1) model.trackPoints

        newTPs =
            precedingTPs ++ remainingTPs

        makeItSo m =
            { m
                | trackPoints = reindexTrackpoints newTPs
                , currentNode = min start (List.length newTPs - 1)
                , markedNode = Nothing
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
                |> checkSceneCamera

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
    -- Note we work here in node/road space and must convert back to lat/lon.
    let
        failed =
            { model
                | smoothedBend = Nothing
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
                    newBend =
                        lookForSmoothBendOption model.bendTrackPointSpacing road1 road2
                in
                case newBend of
                    Just bend ->
                        { model
                            | smoothedBend = Just bend
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
            "gradient smoothing\nfrom "
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
    -- The replacement bend is a pre-computed list of Point3d,
    -- We splice them in as Trackpoints.
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
                    abs (model.currentNode - marker) - 1

                pointsOnArc =
                    -- NOTE bend smoother return includes points A and D
                    List.drop 1 <| List.take (List.length bend.nodes - 1) bend.nodes

                numNewPoints =
                    List.length bend.nodes

                newTrackPoints =
                    nodesToTrackPoints
                        (BoundingBox3d.centerPoint model.trackPointBox)
                        bend.nodes

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
                                List.take (bend.startIndex + 1) m.trackPoints
                                    ++ newTrackPoints
                                    ++ List.drop bend.endIndex m.trackPoints
                        , smoothedBend = Nothing
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

        outputFilename =
            case model.filename of
                Just filename ->
                    filename
                        ++ (if not (String.endsWith ".GPX" (String.toUpper filename)) then
                                ".gpx"

                            else
                                ""
                           )

                Nothing ->
                    "NOFILENAME"
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
        _ = Debug.log "About to parse " content
    in
    { model
        | gpx = Just content
        , trackName = parseTrackName content
        , trackPoints = filterCloseTrackPoints (parseTrackPoints content)
        , changeCounter = 0
    }


deriveNodesAndRoads : Model -> Model
deriveNodesAndRoads model =
    let
        _ = Debug.log "Found track points " model.trackPoints

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

        withMetrics m =
            let
                wrappedNodes =
                    List.map Just m.nodes
            in
            { m
                | nodes =
                    List.map3
                        costMetric
                        (Nothing :: wrappedNodes)
                        m.nodes
                        (List.drop 1 wrappedNodes ++ [ Nothing ])
            }

        costMetric : Maybe DrawingNode -> DrawingNode -> Maybe DrawingNode -> DrawingNode
        costMetric prev this next =
            -- Let's see if area is a good metric.
            -- Maybe just adding bearing and gradient changes is better. Test it.
            case ( prev, next ) of
                ( Just p, Just n ) ->
                    { this
                        | costMetric =
                            Just <|
                                Area.inSquareMeters <|
                                    Triangle3d.area <|
                                        Triangle3d.fromVertices
                                            ( p.location, this.location, n.location )
                    }

                _ ->
                    { this | costMetric = Nothing }
    in
    model
        |> withTrackPointScaling
        |> withNodes
        |> withMetrics
        |> withNodeScaling
        |> withRoads
        |> withSummary
        |> withArrays


resetViewSettings : Model -> Model
resetViewSettings model =
    let
        _ = Debug.log "Resetting the views " model.nodeBox

        focus =
            BoundingBox3d.centerPoint model.nodeBox

        zoomLevel =
            zoomLevelFromBoundingBox model.trackPointBox

        newMapInfo : BoundingBox3d Length.Meters GPXCoords -> MapInfo -> MapInfo
        newMapInfo box info =
            -- If route has changed, make sure mapinfo is up to date.
            { info
                | box = box
                , points = model.trackPoints
                , centreLat = Length.inMeters <| BoundingBox3d.midY box
                , centreLon = Length.inMeters <| BoundingBox3d.midX box
                , mapZoom = zoomLevel
            }

        displayOptions options =
            { options
                | withLighting = Array.length model.nodeArray < 2000
                , roadPillars = Array.length model.nodeArray < 2000
                , roadCones = Array.length model.nodeArray < 2000
                , terrain = False
            }
    in
    { model
        | zoomLevelOverview = zoomLevel
        , zoomLevelFirstPerson = 2.0
        , zoomLevelThirdPerson = zoomLevel
        , zoomLevelProfile = zoomLevel
        , zoomLevelPlan = zoomLevel
        , azimuth = Angle.degrees -90.0
        , elevation = Angle.degrees 30.0
        , currentNode = 0
        , markedNode = Nothing
        , flythrough = Nothing
        , undoStack = []
        , redoStack = []
        , mapInfo = Maybe.map (newMapInfo model.trackPointBox) model.mapInfo
        , cameraFocusThirdPerson = focus
        , cameraFocusProfileNode = 0
        , displayOptions = displayOptions model.displayOptions
    }
        |> checkSceneCamera


checkSceneCamera model =
    { model
        | currentSceneCamera =
            case model.viewingMode of
                FirstPersonView ->
                    firstPersonCamera model

                ThirdPersonView ->
                    thirdPersonCamera model

                ProfileView ->
                    profileCamera model

                PlanView ->
                    planCamera model

                _ ->
                    Nothing
    }


deriveProblems : Model -> Model
deriveProblems model =
    let
        _ = Debug.log "Looking for issues " (List.length model.roads)

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
        _ = Debug.log "Making the 3D forms " model.displayOptions

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
            , smoothedBend = Maybe.withDefault [] <| Maybe.map .nodes model.smoothedBend
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
            , smoothedBend = Maybe.withDefault [] <| Maybe.map .nodes model.smoothedBend
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
        _ = Debug.log "Making moving entities " model.currentNode

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
            , smoothedBend = Maybe.withDefault [] <| Maybe.map .nodes model.smoothedBend
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


stravaRouteOption : Model -> Element Msg
stravaRouteOption model =
    let
        routeIdField =
            Input.text [ width (px 100) ]
                { onChange = UserChangedRouteId
                , text = model.externalRouteId
                , placeholder = Just <| Input.placeholder [] <| E.text "Strava route ID"
                , label = Input.labelHidden "Strava route ID"
                }

        routeButton =
            button
                prettyButtonStyles
                { onPress = Just LoadExternalRoute
                , label = E.text <| "Fetch route"
                }
    in
    case getStravaToken model.stravaAuthentication of
        Just token ->
            row [ spacing 10 ]
                [ routeIdField
                , routeButton
                ]

        Nothing ->
            none


viewAndEditFilename : Model -> Element Msg
viewAndEditFilename model =
    let
        filename =
            Maybe.withDefault "" model.filename
    in
    case model.gpxSource of
        GpxNone ->
            E.none

        _ ->
            column [ Font.size 14 ]
                [ displayName model.trackName
                , Input.text [ width (px 200) ]
                    { onChange = UserChangedFilename
                    , text = filename
                    , placeholder = Nothing
                    , label = Input.labelHidden "File name"
                    }
                ]


view : Model -> Browser.Document Msg
view model =
    { title = "GPXmagic"
    , body =
        [ layout
            [ width fill
            , padding 10
            , spacing 10
            , Font.size 16
            , height fill
            ]
          <|
            column
                []
                [ row [ centerX, spaceEvenly, spacing 10, padding 10 ]
                    [ loadButton
                    , if model.changeCounter == 0 then
                        stravaButton model.stravaAuthentication wrapAuthMessage

                      else
                        E.text "Save your work before\nconnecting to Strava"
                    , stravaRouteOption model
                    , viewAndEditFilename model
                    , saveButtonIfChanged model
                    ]
                , row [ alignLeft, moveRight 200 ]
                    [ if model.gpxSource /= GpxNone then
                        viewModeChoices model

                      else
                        el [ height (px 40) ] none
                    ]
                , case ( model.gpx, model.trackPoints ) of
                    ( _, tp1 :: _ ) ->
                        -- Must have at least one track point.
                        row [ alignLeft, alignTop ]
                            [ view3D model.nodeBox model
                            , column
                                [ width fill, spacing 10, alignTop, centerX ]
                                [ undoButton model
                                , markerButton model
                                , accordionView
                                    (updatedAccordion model model.toolsAccordion toolsAccordion)
                                    AccordionMessage
                                , accordionView
                                    (updatedAccordion model model.infoAccordion infoAccordion)
                                    AccordionMessage
                                ]
                            ]

                    ( Just _, [] ) ->
                        viewInputError model

                    _ ->
                        viewAboutText
                , compatibleWithStrava
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
                , label = E.text "Save as GPX file to your computer"
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
    el [ alignTop, alignLeft ] <|
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
                    [ width <| px viewMapWidth
                    , height <| px viewMapHeight
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
            [ width <| px <| truncate view3dWidth
            , height <| maximum (truncate view3dHeight) fill
            ]
          <|
            if List.length model.trackPoints == 0 then
                case model.gpx of
                    Just content ->
                        column [ spacing 10, padding 10 ]
                            [ E.text "I wanted to see lat, lon and ele data."
                            , E.text "This is what I found instead."
                            , E.text <| content
                            ]

                    Nothing ->
                        E.text "I seem to have no filename."

            else
                E.text "Nothing to see here."
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

        linkButton nodeNum =
            button prettyButtonStyles
                { onPress = Just (UserMovedNodeSlider nodeNum)
                , label = E.text <| String.fromInt nodeNum
                }

        autosmoothButton =
            case nodeList of
                [] ->
                    none

                _ ->
                    button prettyButtonStyles
                        { onPress = Just (AutoFix nodeList)
                        , label = E.text <| "Try AutoFix today"
                        }

        nodeList =
            List.map idx model.abruptGradientChanges
    in
    column [ spacing 5, padding 10 ]
        [ row [ spacing 10 ]
            [ gradientChangeThresholdSlider model
            , autosmoothButton
            ]
        , wrappedRow [ spacing 5, padding 10, width fill, alignLeft ] <|
            List.map linkButton nodeList
        ]


viewBearingChanges : Model -> Element Msg
viewBearingChanges model =
    let
        idx change =
            change.node.trackPoint.idx

        linkButton nodeNum =
            button prettyButtonStyles
                { onPress = Just (UserMovedNodeSlider nodeNum)
                , label = E.text <| String.fromInt nodeNum
                }

        autosmoothButton =
            case nodeList of
                [] ->
                    none

                _ ->
                    button prettyButtonStyles
                        { onPress = Just (AutoFix nodeList)
                        , label = E.text <| "Try Autosmooth today"
                        }

        nodeList =
            List.map idx model.abruptBearingChanges
    in
    column [ spacing 5, padding 10 ]
        [ row [ spacing 10 ]
            [ bearingChangeThresholdSlider model
            , autosmoothButton
            ]
        , wrappedRow [ spacing 5, padding 10, width fill, alignLeft ] <|
            List.map linkButton nodeList
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
        , Font.size 14
        ]
        [ if model.terrainEntities == [] then
            button prettyButtonStyles
                { onPress = Just MakeTerrain
                , label = E.text """Build terrain
(I understand this may take several
minutes and will be lost if I make changes)"""
                }

          else
            button prettyButtonStyles
                { onPress = Just ClearTerrain
                , label = E.text "Remove the terrain."
                }
        , paragraph
            [ padding 10
            ]
          <|
            [ E.text "Select view elements" ]
        , row [ spacing 5 ]
            [ column [ spacing 5 ]
                [ Input.checkbox []
                    { onChange = ToggleRoad
                    , icon = checkboxIcon
                    , checked = model.displayOptions.roadTrack
                    , label = Input.labelRight [ centerY ] (E.text "Road surface")
                    }
                , Input.checkbox []
                    { onChange = ToggleCentreLine
                    , icon = checkboxIcon
                    , checked = model.displayOptions.centreLine
                    , label = Input.labelRight [ centerY ] (E.text "Centre line")
                    }
                ]
            , column [ spacing 5 ]
                [ Input.checkbox []
                    { onChange = TogglePillars
                    , icon = checkboxIcon
                    , checked = model.displayOptions.roadPillars
                    , label = Input.labelRight [ centerY ] (E.text "Road support pillars")
                    }
                , Input.checkbox []
                    { onChange = ToggleCones
                    , icon = checkboxIcon
                    , checked = model.displayOptions.roadCones
                    , label = Input.labelRight [ centerY ] (E.text "Trackpoint cones")
                    }
                ]
            , column [ spacing 5 ]
                [ Input.checkbox []
                    { onChange = ToggleLighting
                    , icon = checkboxIcon
                    , checked = model.displayOptions.withLighting
                    , label = Input.labelRight [ centerY ] (E.text "Lighting")
                    }
                ]
            ]
        , Input.radioRow
            [ Border.rounded 6
            , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
            ]
            { onChange = SetCurtainStyle
            , selected = Just model.displayOptions.curtainStyle
            , label =
                Input.labelBelow [ centerX ] <| E.text "Curtain style"
            , options =
                [ Input.optionWith NoCurtain <| radioButton First "None"
                , Input.optionWith PlainCurtain <| radioButton Mid "Plain"
                , Input.optionWith PastelCurtain <| radioButton Mid "Pastel"
                , Input.optionWith RainbowCurtain <| radioButton Last "Rainbow"
                ]
            }
        ]


viewMapOptions : Model -> Element Msg
viewMapOptions model =
    column
        [ padding 10
        , alignTop
        , spacing 10
        , centerX
        , Font.size 14
        ]
        [ Input.checkbox []
            { onChange = ToggleMapNodesDraggable
            , icon = checkboxIcon
            , checked = model.mapNodesDraggable
            , label = Input.labelRight [ centerY ] (E.text "Drag track points")
            }
        ]


gradientChangeThresholdSlider : Model -> Element Msg
gradientChangeThresholdSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetGradientChangeThreshold
        , label =
            Input.labelBelow [] <|
                E.text <|
                    "Gradient change threshold = "
                        ++ showDecimal2 model.gradientChangeThreshold
        , min = 5.0
        , max = 20.0
        , step = Just 1.0
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
                E.text <|
                    "Direction change threshold = "
                        ++ String.fromInt model.bearingChangeThreshold
        , min = 20.0
        , max = 120.0
        , step = Just 1.0
        , value = toFloat model.bearingChangeThreshold
        , thumb = Input.defaultThumb
        }


bendSmoothnessSlider : Model -> Element Msg
bendSmoothnessSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetBendTrackPointSpacing
        , label =
            Input.labelBelow [] <|
                E.text <|
                    "Spacing = "
                        ++ showDecimal2 model.bendTrackPointSpacing
        , min = 1.0
        , max = 10.0
        , step = Nothing
        , value = model.bendTrackPointSpacing
        , thumb = Input.defaultThumb
        }


updatedAccordion model currentAccordion referenceAccordion =
    -- We have to reapply the accordion update functions with the current model,
    let
        blendAccordionStatus currentAccordionState refreshedContent =
            { currentAccordionState | content = refreshedContent.content }
    in
    List.map2
        blendAccordionStatus
        currentAccordion
        (referenceAccordion model)


overviewSummary model =
    case model.summary of
        Just summary ->
            row [ padding 20, centerX ]
                [ column [ spacing 10 ]
                    [ E.text "Highest point "
                    , E.text "Lowest point "
                    , E.text "Track length "
                    , E.text "Climbing distance "
                    , E.text "Elevation gain "
                    , E.text "Descending distance "
                    , E.text "Elevation loss "
                    ]
                , column [ spacing 10 ]
                    [ E.text <| showDecimal2 summary.highestMetres
                    , E.text <| showDecimal2 summary.lowestMetres
                    , E.text <| showDecimal2 summary.trackLength
                    , E.text <| showDecimal2 summary.climbingDistance
                    , E.text <| showDecimal2 summary.totalClimbing
                    , E.text <| showDecimal2 summary.descendingDistance
                    , E.text <| showDecimal2 summary.totalDescending
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
                    E.text <|
                        "Make the track into a loop"
                }

        reverseButton =
            button
                prettyButtonStyles
                { onPress = Just ReverseTrack
                , label =
                    E.text <|
                        "Reverse the track"
                }

        changeStartButton c =
            button
                prettyButtonStyles
                { onPress = Just (ChangeLoopStart c)
                , label =
                    E.text <|
                        "Move start/finish to current point"
                }

        commonButtons =
            wrappedRow [ spacing 10, padding 20, centerX ]
                [ reverseButton
                ]
    in
    column [ spacing 10, padding 5, centerX ] <|
        case model.loopiness of
            IsALoop ->
                [ row [ spacing 10, padding 5, centerX ]
                    [ E.text "This track is a loop."
                    , changeStartButton model.currentNode
                    ]
                , commonButtons
                ]

            AlmostLoop gap ->
                [ row [ spacing 10, padding 5, centerX ]
                    [ E.text <| "This track is " ++ showDecimal2 gap ++ "\naway from a loop"
                    , loopButton
                    ]
                , commonButtons
                ]

            NotALoop gap ->
                [ row [ spacing 10, padding 5, centerX ]
                    [ E.text <| "This track is " ++ showDecimal2 gap ++ " away from a loop"
                    , loopButton
                    ]
                , commonButtons
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
        [ height <| px scrollbarThickness
        , width <| px 300
        , centerY
        , behindContent <|
            -- Slider track
            el
                [ width <| px 300
                , height <| px scrollbarThickness
                , centerY
                , centerX
                , Background.color scrollbarBackground
                , Border.rounded 6
                ]
                E.none
        ]
        { onChange = UserMovedNodeSlider << round
        , label =
            Input.labelHidden "Drag slider or use arrow buttons"
        , min = 1.0
        , max = toFloat <| Array.length model.roadArray - 1
        , step = Just 1
        , value = toFloat model.currentNode
        , thumb = Input.defaultThumb
        }


viewFirstPerson model =
    row [ alignTop ]
        [ column
            [ alignTop
            ]
            [ viewRoadSegment model
            , positionControls model
            ]
        ]


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
                        E.text <|
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
    column [ padding 10, spacing 10, centerX ]
        [ row [ padding 10, spacing 10, centerX ]
            [ resetButton
            , playPauseButton
            , flythroughSpeedSlider
            ]
        , case model.flythrough of
            Just flythrough ->
                row [ spacing 10 ]
                    [ column [ spacing 10 ]
                        [ E.text "Segment "
                        , E.text "Metres from start "
                        ]
                    , column [ spacing 10 ]
                        [ E.text <| String.fromInt flythrough.segment.index
                        , E.text <| showDecimal2 flythrough.metresFromRouteStart
                        ]
                    ]

            Nothing ->
                none
        ]


firstPersonCamera : Model -> Maybe (Camera3d Length.Meters LocalCoords)
firstPersonCamera model =
    let
        eyePoint road =
            case model.flythrough of
                Nothing ->
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 eyeHeight)
                        road.startsAt.location

                Just flying ->
                    flying.cameraPosition

        cameraViewpoint road =
            case model.flythrough of
                Nothing ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint road
                        , focalPoint =
                            Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 eyeHeight)
                                road.endsAt.location
                        , upDirection = Direction3d.positiveZ
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint road
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }

        cappedNodeNumber =
            min model.currentNode (Array.length model.nodeArray - 2)
    in
    case Array.get cappedNodeNumber model.roadArray of
        Just road ->
            Just <|
                Camera3d.perspective
                    { viewpoint = cameraViewpoint road
                    , verticalFieldOfView = Angle.degrees <| 120.0 / (1 + model.zoomLevelFirstPerson / 2.0)
                    }

        Nothing ->
            Nothing


viewRoadSegment : Model -> Element Msg
viewRoadSegment model =
    case model.currentSceneCamera of
        Just camera ->
            row [ padding 5, spacing 10 ]
                [ el
                    withMouseCapture
                  <|
                    html <|
                        render3dScene model.displayOptions.withLighting
                            { camera = camera
                            , dimensions = view3dDimensions
                            , background = Scene3d.backgroundColor Color.lightBlue
                            , clipDepth = Length.meters 1.0
                            , entities =
                                model.varyingVisualEntities
                                    ++ model.staticVisualEntities
                                    ++ model.terrainEntities
                            }
                , zoomSlider model.zoomLevelFirstPerson ZoomLevelFirstPerson
                ]

        Nothing ->
            none


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
                    [ viewCurrentNode model
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
                    E.text <|
                        "Smooth between markers\nRadius "
                            ++ showDecimal2 smooth.radius
                }
    in
    column [ spacing 10, padding 10, alignTop, centerX ]
        [ case model.smoothedBend of
            Just smooth ->
                row [ spacing 10, padding 10, alignTop ]
                    [ fixBendButton smooth
                    , bendSmoothnessSlider model
                    ]

            Nothing ->
                column [ spacing 10, padding 10, alignTop, centerX ]
                    [ E.text "Sorry, failed to find a nice bend."
                    , E.text "Try re-positioning the current pointer or marker."
                    ]
        ]


viewStraightenTools : Model -> Element Msg
viewStraightenTools model =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        simplifyButton =
            button
                prettyButtonStyles
                { onPress = Just SimplifyTrack
                , label =
                    E.text <|
                        "Remove up to "
                            ++ String.fromInt (List.length model.metricFilteredNodes)
                            ++ " track points\nto simplify the route."
                }
    in
    column [ spacing 10, padding 10, alignTop, centerX ]
        [ if model.currentNode /= marker then
            straightenButton

          else
            column [ spacing 10, padding 10, alignTop, centerX ]
                [ E.text "The straighten tool requires a range."
                , E.text "Drop the marker and move it away from the current pointer."
                ]
        , simplifyButton
        , E.text "Simplify works across a range if available,\notherwise the whole track."
        ]


viewNudgeTools : Model -> Element Msg
viewNudgeTools model =
    --2020-12-08 Adding tools to Nudge node, split straight, straighten straight.
    column [ padding 5, spacing 10, centerX ]
        [ row [ spacing 10, centerX ]
            [ verticalNudgeSlider model.verticalNudgeValue
            , column [ spacing 10, centerX, centerY ]
                [ horizontalNudgeSlider model.nudgeValue
                , nudgeButton model.nudgeValue model.verticalNudgeValue
                ]
            ]
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
                    E.text <| label
                }
    in
    row
        [ padding 5
        , spacing 10
        , Border.width 0
        , Border.rounded 5
        , width fill
        , centerX
        , Font.size 16
        , Background.color buttonGroupBackground
        ]
    <|
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
    row
        [ padding 5
        , spacing 10
        , Border.width 0
        , Border.rounded 5
        , width fill
        , centerX
        , Background.color buttonGroupBackground
        ]
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
                        E.text <| "Undo " ++ u.label

                    _ ->
                        E.text "Nothing to undo"
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
                        E.text <| "Redo " ++ u.label

                    _ ->
                        E.text "Nothing to redo"
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
                    row [ spacing 5, padding 5 ]
                        [ button
                            prettyButtonStyles
                            { onPress = Just <| SmoothGradient gradient
                            , label =
                                E.text <|
                                    "Smooth between markers\nAverage gradient "
                                        ++ showDecimal2 gradient
                            }
                        , smoothnessSlider model
                        ]

                _ ->
                    column [ spacing 10, padding 10, alignTop, centerX ]
                        [ E.text "Gradient smoothing works over a range of track segments."
                        , E.text "Try re-positioning the current pointer or marker."
                        ]
    in
    column [ padding 10, spacing 10, centerX ] <|
        [ gradientSmoothControls ]


wholeTrackTextHelper model =
    let
        wholeTrack =
            case model.markedNode of
                Just m ->
                    m == model.currentNode

                Nothing ->
                    True

        isLoop =
            model.loopiness == IsALoop
    in
    row [ padding 5, spacing 10, Background.color warningColor, width fill ]
        [ html <| FeatherIcons.toHtml [] FeatherIcons.info
        , case ( wholeTrack, isLoop ) of
            ( True, True ) ->
                column []
                    [ E.text "Applies to the whole route and include the start/finish."
                    , E.text "Position markers to apply only to a range of points."
                    ]

            ( True, False ) ->
                column []
                    [ E.text "Applies to the whole route but not the start/finish."
                    , E.text "Position markers to apply only to a range of points."
                    , E.text "Convert the track to a loop to smooth the start/finish."
                    ]

            ( False, True ) ->
                column []
                    [ E.text "Applies between the marker cones, avoiding the start/finish."
                    , E.text "Clear the marker to apply to the whole route."
                    ]

            ( False, False ) ->
                column []
                    [ E.text "Applies between the marker cones only."
                    , E.text "Clear the marker to apply to the whole route."
                    ]
        ]


viewTrackPointTools : Model -> Element Msg
viewTrackPointTools model =
    let
        marker =
            Maybe.withDefault model.currentNode model.markedNode

        ( start, finish ) =
            ( min model.currentNode marker
            , max model.currentNode marker
            )
    in
    column [ padding 10, spacing 10, centerX ] <|
        [ row [ spacing 20 ]
            [ insertNodeOptionsBox model.currentNode
            , deleteNodeButton ( start, finish )
            ]
        , splitSegmentOptions model.maxSegmentSplitSize
        , wholeTrackTextHelper model
        ]


insertNodeOptionsBox c =
    row [ spacing 10 ]
        [ button
            prettyButtonStyles
            { onPress = Just (InsertBeforeOrAfter c InsertNodeAfter)
            , label = E.text "Put two trackpoints in\nplace of this one"
            }

        --, button
        --    prettyButtonStyles
        --    { onPress = Just (InsertBeforeOrAfter c InsertNodeBefore)
        --    , label = E.text "Insert a node\nbefore this one"
        --    }
        ]


smoothnessSlider : Model -> Element Msg
smoothnessSlider model =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = SetBumpinessFactor
        , label =
            Input.labelBelow [] <|
                E.text <|
                    "Bumpiness = "
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


thirdPersonCamera : Model -> Maybe (Camera3d Length.Meters LocalCoords)
thirdPersonCamera model =
    let
        latitude =
            degrees <| Length.inMeters <| BoundingBox3d.midY model.trackPointBox

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = focalPoint
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters <| 1200.0 * metresPerPixel model.zoomLevelThirdPerson latitude
                        }
                , verticalFieldOfView = Angle.degrees 30.0
                }

        focalPoint =
            case model.flythrough of
                Nothing ->
                    model.cameraFocusThirdPerson

                Just flying ->
                    flying.cameraPosition
    in
    Just camera


viewCurrentNode : Model -> Element Msg
viewCurrentNode model =
    case model.currentSceneCamera of
        Just camera ->
            row [ padding 5, spacing 10 ]
                [ el
                    withMouseCapture
                  <|
                    html <|
                        render3dScene model.displayOptions.withLighting
                            { camera = camera
                            , dimensions = view3dDimensions
                            , background = Scene3d.backgroundColor Color.lightBlue
                            , clipDepth = Length.meters 1.0
                            , entities =
                                model.varyingVisualEntities
                                    ++ model.staticVisualEntities
                                    ++ model.terrainEntities
                            }
                , zoomSlider model.zoomLevelThirdPerson ZoomLevelThirdPerson
                ]

        Nothing ->
            none


planCamera : Model -> Maybe (Camera3d Length.Meters LocalCoords)
planCamera model =
    let
        latitude =
            degrees <| Length.inMeters <| BoundingBox3d.midY model.trackPointBox

        focus =
            case model.flythrough of
                Nothing ->
                    Point3d.projectOnto Plane3d.xy
                        model.cameraFocusPlan

                Just flying ->
                    Point3d.projectOnto
                        Plane3d.xy
                        flying.cameraPosition

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                focus

        camera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = focus
                        , eyePoint = eyePoint
                        , upDirection = positiveY
                        }
                , viewportHeight = Length.meters <| 1200.0 * metresPerPixel model.zoomLevelPlan latitude
                }
    in
    Just camera


viewCurrentNodePlanView : Model -> DrawingNode -> Element Msg
viewCurrentNodePlanView model node =
    case model.currentSceneCamera of
        Just camera ->
            row [ padding 5, spacing 10 ]
                [ el withMouseCapture <|
                    html <|
                        render3dScene False
                            { camera = camera
                            , dimensions = view3dDimensions
                            , background = Scene3d.backgroundColor Color.darkGreen
                            , clipDepth = Length.meters 1.0
                            , entities = model.varyingVisualEntities ++ model.staticVisualEntities
                            }
                , zoomSlider model.zoomLevelPlan ZoomLevelPlan
                ]

        Nothing ->
            none


profileCamera : Model -> Maybe (Camera3d Length.Meters LocalCoords)
profileCamera model =
    let
        latitude =
            degrees <| Length.inMeters <| BoundingBox3d.midY model.trackPointBox

        trackLength =
            Maybe.withDefault 10000.0 <|
                Maybe.map .trackLength model.summary

        focus node =
            case model.flythrough of
                Nothing ->
                    Point3d.projectOnto Plane3d.yz node.location

                Just flying ->
                    let
                        fixForFlythrough =
                            Point3d.toRecord inMeters node.location
                    in
                    Point3d.projectOnto
                        Plane3d.yz
                        (Point3d.fromRecord meters { fixForFlythrough | y = flying.metresFromRouteStart })

        eyePoint node =
            Point3d.translateBy
                (Vector3d.meters 100.0 0.0 0.0)
                (focus node)

        camera road =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = focus road.profileStartsAt
                        , eyePoint = eyePoint road.profileStartsAt
                        , upDirection = positiveZ
                        }
                , viewportHeight =
                    Length.meters <|
                        metresPerPixel model.zoomLevelProfile latitude
                            * (trackLength / viewMapWidth)
                }
    in
    Maybe.map camera (Array.get model.cameraFocusProfileNode model.roadArray)


viewRouteProfile : Model -> DrawingNode -> Element Msg
viewRouteProfile model node =
    case model.currentSceneCamera of
        Just camera ->
            row [ padding 5, spacing 10 ]
                [ el
                    withMouseCapture
                  <|
                    html <|
                        render3dScene False
                            { camera = camera
                            , dimensions = view3dDimensions
                            , background = Scene3d.backgroundColor Color.lightCharcoal
                            , clipDepth = Length.meters 1.0
                            , entities = model.varyingProfileEntities ++ model.staticProfileEntities
                            }
                , zoomSlider model.zoomLevelProfile ZoomLevelProfile
                ]

        Nothing ->
            none


viewStravaDataAccessTab : Model -> Element Msg
viewStravaDataAccessTab model =
    let
        segmentIdField =
            Input.text []
                { onChange = UserChangedSegmentId
                , text = model.externalSegmentId
                , placeholder = Just <| Input.placeholder [] <| E.text "Segment ID"
                , label = Input.labelHidden "Segment ID"
                }

        segmentButton =
            -- Make this button serve two functions.
            -- 1. After a URL change, to load the segment header;
            -- 2. After header loaded, to load and paste the streams.
            case model.externalSegment of
                SegmentOk segment ->
                    let
                        pStartingTrackPoint =
                            trackPointFromLatLon
                                segment.start_latitude
                                segment.start_longitude
                                model.trackPoints

                        pEndingTrackPoint =
                            trackPointFromLatLon
                                segment.end_latitude
                                segment.end_longitude
                                model.trackPoints

                        buttonText =
                            case ( pStartingTrackPoint, pEndingTrackPoint ) of
                                ( Just start, Just finish ) ->
                                    if start.idx < finish.idx then
                                        "Paste segment into route"

                                    else
                                        "Reverse segment and paste into route"

                                _ ->
                                    "Oh! Can't work out how to paste this."
                    in
                    button
                        prettyButtonStyles
                        { onPress = Just LoadSegmentStreams
                        , label = E.text buttonText
                        }

                SegmentNone ->
                    button
                        prettyButtonStyles
                        { onPress = Just LoadExternalSegment
                        , label = E.text <| "Fetch header"
                        }

                SegmentNotInRoute _ ->
                    E.text "This segment is not\ncontained in the route"

                _ ->
                    E.none

        segmentInfo =
            case model.externalSegment of
                SegmentRequested ->
                    E.text "Waiting for segment"

                SegmentError err ->
                    E.text err

                SegmentNone ->
                    E.text "Segment data not loaded, or not yet."

                SegmentOk segment ->
                    E.text segment.name

                SegmentNotInRoute segment ->
                    E.text segment.name

        stravaLink =
            let
                stravaUrl =
                    Builder.crossOrigin stravaApiRoot [ "routes", model.externalRouteId ] []
            in
            if model.gpxSource == GpxStrava then
                column [ Font.size 14, padding 5 ]
                    [ displayName model.trackName
                    , E.newTabLink [ Font.color stravaOrange ]
                        { url = stravaUrl
                        , label = E.text "View on Strava"
                        }
                    ]

            else
                E.none
    in
    column [ spacing 10, padding 10, width fill ]
        [ stravaLink
        , row [ spacing 10 ]
            [ segmentIdField
            , segmentButton
            ]
        , segmentInfo
        ]


viewFilterControls : Model -> Element Msg
viewFilterControls model =
    let
        ( filterXY, filterZ ) =
            model.filterModes
    in
    column [ spacing 10, padding 10 ]
        [ E.text "Smooth the track by applying some filters."
        , E.text "Better results may be achieved by inserting track points first."
        , E.text "Note that repeatedly applying approximates to Gaussian smoothing."
        , row [ padding 3, spacing 3 ]
            [ Input.checkbox []
                { onChange = ToggleFilterXY
                , icon = checkboxIcon
                , checked = filterXY
                , label = Input.labelRight [ centerY ] (E.text "Filter latitude & longitude")
                }
            , Input.checkbox []
                { onChange = ToggleFilterZ
                , icon = checkboxIcon
                , checked = filterZ
                , label = Input.labelRight [ centerY ] (E.text "Filter elevation")
                }
            ]
        , case model.filterModes of
            ( True, False ) ->
                E.text "That might be weird, but it's your call."

            ( False, False ) ->
                E.text "You know that won't do anything."

            _ ->
                E.none
        , button
            prettyButtonStyles
            { onPress = Just FilterWeightedAverage
            , label = E.text <| "Five point weighted average"
            }
        , wholeTrackTextHelper model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ messageReceiver MapMessage
        , mapStopped MapRemoved
        , Time.every 50 Tick
        , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
        ]
