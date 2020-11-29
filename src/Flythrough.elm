module Flythrough exposing (..)

import Array
import NodesAndRoads exposing (DrawingRoad)
import ScalingInfo exposing (ScalingInfo)
import Time


type alias Flythrough =
    { cameraPosition : ( Float, Float, Float )
    , focusPoint : ( Float, Float, Float )
    , metresFromRouteStart : Float
    , lastUpdated : Time.Posix
    , running : Bool
    , segment : DrawingRoad
    }


flythrough :
    Time.Posix
    -> Flythrough
    -> Float
    -> List DrawingRoad
    -> Flythrough
flythrough newTime flying speed roads =
    let
        findRoadByDistance d rs =
            case rs of
                [] ->
                    ( Nothing, Nothing )

                [ r1 ] ->
                    if d < r1.endDistance then
                        ( Just r1, Nothing )

                    else
                        ( Nothing, Nothing )

                r1 :: r2 :: rN ->
                    if d >= r1.startDistance && d < r1.endDistance then
                        ( Just r1, Just r2 )

                    else
                        findRoadByDistance d (r2 :: rN)

        newDistance =
            flying.metresFromRouteStart + tempus * 10.0 ^ speed

        tempus =
            toFloat (Time.posixToMillis newTime - Time.posixToMillis flying.lastUpdated) / 1000.0

        ( currentSegment, nextSeg ) =
            findRoadByDistance newDistance roads
    in
    if flying.running then
        case currentSegment of
            Just seg ->
                let
                    segInsetMetres =
                        newDistance - seg.startDistance

                    segFraction =
                        segInsetMetres / seg.length

                    segRemaining =
                        seg.length - segInsetMetres

                    headTurnFraction =
                        -- Allow for POV rotation as we near segment end.
                        clamp 0.0 1.0 (10.0 - segRemaining) / 10.0

                    ( cameraX, cameraY, cameraZ ) =
                        -- New location of rider
                        ( segFraction * seg.endsAt.x + (1 - segFraction) * seg.startsAt.x
                        , segFraction * seg.endsAt.y + (1 - segFraction) * seg.startsAt.y
                        , segFraction * seg.endsAt.z + (1 - segFraction) * seg.startsAt.z
                        )

                    lookingAt =
                        case nextSeg of
                            Just next ->
                                let
                                    ( nextX, nextY, nextZ ) =
                                        -- Near the end, start looking at the next segment.
                                        ( (next.startsAt.x + next.endsAt.x) / 2.0
                                        , (next.startsAt.y + next.endsAt.y) / 2.0
                                        , (next.startsAt.z + next.endsAt.z) / 2.0
                                        )

                                    ( focusX, focusY, focusZ ) =
                                            ( nextX * headTurnFraction + (1.0 - headTurnFraction) * seg.endsAt.x
                                            , nextY * headTurnFraction + (1.0 - headTurnFraction) * seg.endsAt.y
                                            , nextZ * headTurnFraction + (1.0 - headTurnFraction) * seg.endsAt.z
                                            )
                                in
                                ( focusX, focusY, focusZ )

                            Nothing ->
                                ( seg.endsAt.x, seg.endsAt.y, seg.endsAt.z )
                in
                { flying
                    | metresFromRouteStart = newDistance
                    , lastUpdated = newTime
                    , segment = seg
                    , cameraPosition = ( cameraX, cameraY, cameraZ )
                    , focusPoint = lookingAt
                }

            Nothing ->
                { flying | running = False }

    else
        { flying | running = False }
