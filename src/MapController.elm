port module MapController exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Element exposing (Element, text)
import Json.Decode exposing (Decoder, decodeValue, field, string)
import Json.Encode as E
import Length
import MapboxKey exposing (mapboxKey)
import Msg exposing (Msg(..))
import NodesAndRoads exposing (GPXCoords)
import Point3d
import TrackPoint exposing (TrackPoint, trackToJSON)


type MapState
    = WaitingForNode
    | WaitingForMapLoad
    | MapLoaded
    | TrackLoaded
    | MapStopping
    | MapStopped


type
    MapMessage
    -- These will arrive through the inbound Port and update the state machine.
    = NodeAvailable
    | MapHasLoaded
    | TrackHasLoaded
    | MapHasStopped


type alias MapInfo =
    { mapState : MapState
    , box : BoundingBox3d Length.Meters GPXCoords
    , points : List TrackPoint
    }


port mapPort : E.Value -> Cmd msg


port messageReceiver : (E.Value -> msg) -> Sub msg


createMap : MapInfo -> Cmd Msg
createMap info =
    let
        centre =
            BoundingBox3d.centerPoint info.box
    in
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Length.inMeters <| Point3d.xCoordinate centre )
            , ( "lat", E.float <| Length.inMeters <| Point3d.yCoordinate centre )
            , ( "zoom", E.float 12.0 )
            ]


addTrackToMap : MapInfo -> Cmd Msg
addTrackToMap info =
    let
        centre =
            BoundingBox3d.centerPoint info.box
    in
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "lon", E.float <| Length.inMeters <| Point3d.xCoordinate centre )
            , ( "lat", E.float <| Length.inMeters <| Point3d.yCoordinate centre )
            , ( "zoom", E.float 12.0 )
            , ( "data", trackToJSON info.points )
            ]


viewMapInfo : Maybe MapInfo -> Element Msg
viewMapInfo mapInfo =
    case mapInfo of
        Just info ->
            text <|
                case info.mapState of
                    WaitingForNode ->
                        "Waiting for node"

                    WaitingForMapLoad ->
                        "Waiting for map to load"

                    MapLoaded ->
                        "Map loaded"

                    TrackLoaded ->
                        "Track loaded"

                    MapStopping ->
                        "Stopping"

                    MapStopped ->
                        "Stopped"

        Nothing ->
            text "There is no map information"


processMapMessage : MapInfo -> E.Value -> ( MapInfo, Cmd Msg )
processMapMessage info json =
    -- Assume it's the map ready, initially
    let
        msg =
            decodeValue msgDecoder json
    in
    case msg of
        Ok "no node" ->
            ( { info | mapState = WaitingForNode }
            , createMap info
            )

        Ok "map ready" ->
            ( { info | mapState = MapLoaded }
            , addTrackToMap info
            )

        _ ->
            ( info, Cmd.none )


msgDecoder : Decoder String
msgDecoder =
    field "msg" string
