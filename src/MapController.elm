port module MapController exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Element exposing (Element, centerX, column, padding, row, spacing, text)
import Json.Decode exposing (Decoder, decodeValue, field, float, string)
import Json.Encode as E
import Length
import MapboxKey exposing (mapboxKey)
import Msg exposing (Msg(..))
import NodesAndRoads exposing (GPXCoords)
import Point3d
import TrackPoint exposing (TrackPoint, trackToJSON)
import Utils exposing (showDecimal2, showDecimal6)
import ViewTypes exposing (ViewingMode)


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
    --TODO: Track Centre and Zoom for restore when map is re-created.
    { mapState : MapState
    , box : BoundingBox3d Length.Meters GPXCoords
    , points : List TrackPoint
    , nextView : ViewingMode -- deferred view change while map is removed.
    , centreLon : Float -- track values from user map interactions.
    , centreLat : Float -- track values from user map interactions.
    , mapZoom : Float -- track values from user map interactions.
    }


port mapPort : E.Value -> Cmd msg


port messageReceiver : (E.Value -> msg) -> Sub msg


port mapStopped : (String -> msg) -> Sub msg


createMap : MapInfo -> Cmd Msg
createMap info =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            ]


removeMap : Cmd Msg
removeMap =
    mapPort <| E.object [ ( "Cmd", E.string "Stop" ) ]


addTrackToMap : MapInfo -> Cmd Msg
addTrackToMap info =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            , ( "data", trackToJSON info.points )
            ]


decodeState state =
    text <|
        case state of
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


viewMapInfo : Maybe MapInfo -> Element Msg
viewMapInfo mapInfo =
    case mapInfo of
        Just info ->
            column [ padding 10, spacing 10, centerX ]
                [ decodeState info.mapState
                , row [ padding 10, spacing 10, centerX ]
                    [ column [ padding 10, spacing 10, centerX ]
                        [ text "Longitude "
                        , text "Latitude "
                        , text "Zoom "
                        ]
                    , column [ padding 10, spacing 10, centerX ]
                        [ text <| showDecimal6 info.centreLon
                        , text <| showDecimal6 info.centreLat
                        , text <| showDecimal2 info.mapZoom
                        ]
                    ]
                ]

        Nothing ->
            column [ padding 10, spacing 10, centerX ]
                [ text "Map information is available only once a map has been loaded." ]


processMapMessage : MapInfo -> E.Value -> ( MapInfo, Cmd Msg )
processMapMessage info json =
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

        Ok "move" ->
            --( { 'msg' : 'move'
            --  , 'lat' : map.getCentre().lat
            --  , 'lon' : map.getCentre().lon
            --  , 'zoom' : map.getZoom()
            --  } );
            let
                lat =
                    decodeValue (field "lat" float) json

                lon =
                    decodeValue (field "lon" float) json

                zoom =
                    decodeValue (field "zoom" float) json
            in
            case ( lat, lon, zoom ) of
                ( Ok lat1, Ok lon1, Ok zoom1 ) ->
                    ( { info
                        | centreLon = lon1
                        , centreLat = lat1
                        , mapZoom = zoom1
                      }
                    , Cmd.none
                    )

                _ ->
                    ( info, Cmd.none )

        _ ->
            ( info, Cmd.none )


msgDecoder : Decoder String
msgDecoder =
    field "msg" string