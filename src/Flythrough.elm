module Flythrough exposing (..)

import Length
import NodesAndRoads exposing (DrawingRoad, MyCoord)
import Point3d exposing (Point3d)
import Time


type alias Flythrough =
    { cameraPosition : Point3d Length.Meters MyCoord
    , focusPoint : Point3d Length.Meters MyCoord
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

                    camera3d =
                        Point3d.interpolateFrom
                            seg.startsAt.location
                            seg.endsAt.location
                            segFraction

                    lookingAt =
                        case nextSeg of
                            Just next ->
                                let
                                    next3d =
                                        Point3d.interpolateFrom
                                            next.startsAt.location
                                            next.endsAt.location
                                            0.5
                                in
                                Point3d.interpolateFrom
                                    seg.endsAt.location
                                    next3d
                                    headTurnFraction

                            Nothing ->
                                seg.endsAt.location
                in
                { flying
                    | metresFromRouteStart = newDistance
                    , lastUpdated = newTime
                    , segment = seg
                    , cameraPosition = camera3d
                    , focusPoint = lookingAt
                }

            Nothing ->
                { flying | running = False }

    else
        { flying | running = False }
