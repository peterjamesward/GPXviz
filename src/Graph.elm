module Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.
-- I'm going to migrate current functionality to this before creating the editing suite.
-- OBSERVATIONS. Each non-node trackpoint belongs to one edge.
-- When we draw the graph, the user needs to be able to select edges,
-- both for editing and for adding to a route.
-- Thus, each non-node TP must have an edge number.
-- Edges then referenced by number, as are nodes.
-- Once we've canonicalised, everything is by the numbers.
-- For completeness, each trackpoint is either a Node (optionally S/F) point or an Edge point.
-- Backwards compatibility? = After loading, there is one edge from S to F.
-- Or are we modal, as so much changes (visuals, editing tools) ??
--TODO: Check if last TP = first TP => same node (loop). Or that comes out in the wash?

import Angle
import Axis3d
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Direction3d
import Element as E exposing (Element, alignTop, centerX, padding, spacing)
import Element.Input as I
import Length
import List.Extra as List
import Point3d exposing (Point3d)
import Set exposing (Set)
import TrackPoint exposing (GPXCoords, TrackPoint, fromGPXcoords, reindexTrackpoints, toGPXcoords, trackPointSeparation)
import UbiquitousTypes exposing (LocalCoords)
import Utils exposing (showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


type Direction
    = Forwards
    | Backwards


type alias LatLon =
    ( Float, Float )


type alias Graph =
    { nodes : Dict Int TrackPoint
    , edges : Dict Int Edge
    , route : List Traversal
    , centreLineOffset : Float
    , index : Dict Int PointType
    , boundingBox : BoundingBox3d Length.Meters GPXCoords -- ugly
    , trackPoints : List TrackPoint
    }


type
    PointType
    -- We shall use this to build an index back from Trackpoint land to Graph land.
    = StartPoint Int
    | FinishPoint Int
    | NodePoint Int
    | EdgePoint Int Int -- Store both the edge number and the unique number (which might be locally unique).


empty =
    { nodes = Dict.empty
    , edges = Dict.empty
    , route = []
    , centreLineOffset = 0.0
    , index = Dict.empty
    , boundingBox = BoundingBox3d.singleton Point3d.origin
    , trackPoints = []
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset Float
    | ApplyOffset


type alias Node =
    TrackPoint


type alias Edge =
    { startNode : Int
    , endNode : Int
    , wayPoint : LatLon
    , trackPoints : List TrackPoint
    }


type alias Traversal =
    { edge : Int
    , direction : Direction
    }


type alias Route =
    { route : List Traversal }


viewGraphControls : Graph -> (Int, Int) -> (Msg -> msg) -> Element msg
viewGraphControls graph (current, marker) wrapper =
    let
        analyseButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper GraphAnalyse)
                , label = E.text "Analyse"
                }

        offsetSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << CentreLineOffset
                , label =
                    I.labelBelow [] <|
                        E.text <|
                            "Offset = "
                                ++ (showDecimal2 <| abs graph.centreLineOffset)
                                ++ "m "
                                ++ (if graph.centreLineOffset < 0.0 then
                                        "left"

                                    else if graph.centreLineOffset > 0.0 then
                                        "right"

                                    else
                                        ""
                                   )
                , min = -5.0
                , max = 5.0
                , step = Just 1.0
                , value = graph.centreLineOffset
                , thumb = I.defaultThumb
                }

        applyOffsetButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper ApplyOffset)
                , label = E.text "Apply offset"
                }

        markerInfo =
            let
                canonicalPoints =
                    withinSameEdge graph (current, marker)
            in
            E.column []
                [ E.text <| "Track index " ++ String.fromInt current
                , E.text <| "Marker cone " ++ String.fromInt marker
                , case canonicalPoints of
                    Just ( n1, n2 ) ->
                        E.text <| "Canonical " ++ String.fromInt n1 ++ ", " ++ String.fromInt n2

                    _ ->
                        E.text "No entry in canon"
                ]
    in
    E.row []
        [ E.column [ spacing 10, padding 10, centerX ]
            [ analyseButton
            , offsetSlider
            , applyOffsetButton
            ]
        , markerInfo
        ]


update :
    Msg
    -> { a | trackPoints : List TrackPoint, graph : Graph, trackPointBox : BoundingBox3d Length.Meters GPXCoords }
    -> ( Graph, Maybe String )
update msg model =
    case msg of
        GraphAnalyse ->
            ( deriveTrackPointGraph model.trackPoints model.trackPointBox
            , Just "Canonicalised edges"
            )

        CentreLineOffset offset ->
            let
                newGraph g =
                    -- This syntax seems necessary.
                    { g | centreLineOffset = offset }
            in
            ( newGraph model.graph, Nothing )

        ApplyOffset ->
            ( model.graph, Just "Apply offset" )


deriveTrackPointGraph :
    List TrackPoint
    -> BoundingBox3d Length.Meters GPXCoords
    -> Graph
deriveTrackPointGraph trackPoints box =
    let
        --trackPoints =
        --    -- Might help to avoid false nodes.
        --    List.filter (\tp -> tp.costMetric > 0) unfilteredTrackPoints
        rawNodes =
            interestingTrackPoints trackPoints

        rawEdges =
            findDistinctEdges rawNodes trackPoints

        canonicalEdges =
            findCanonicalEdges rawEdges

        canonicalRoute : List Traversal
        canonicalRoute =
            useCanonicalEdges rawEdges canonicalEdges
                |> List.map
                    (\( key, direction ) ->
                        let
                            edge =
                                Dict.get key edgeKeysToIndex
                        in
                        case edge of
                            Just e ->
                                Just
                                    { edge = e
                                    , direction = direction
                                    }

                            Nothing ->
                                Nothing
                    )
                |> List.filterMap identity

        nodeCoordinatesToIndex : Dict LatLon Int
        nodeCoordinatesToIndex =
            -- Assign nodes sequential index numbers and allow conversion from position.
            Dict.fromList <|
                List.map2
                    (\key idx -> ( key, idx ))
                    (Dict.keys rawNodes)
                    (List.range 1 (Dict.size rawNodes))

        edgeKeysToIndex : Dict ( LatLon, LatLon, LatLon ) Int
        edgeKeysToIndex =
            -- Assign sequential IDs to edges and allow conversion from positions.
            Dict.fromList <|
                List.map2
                    (\key idx -> ( key, idx ))
                    (Dict.keys canonicalEdges)
                    (List.range 1 (Dict.size canonicalEdges))

        indexedNodes : Dict Int TrackPoint
        indexedNodes =
            -- Convert from a Dict keyed on position to one keyed by Int.
            List.map
                (\( k, v ) -> ( Dict.get k nodeCoordinatesToIndex, v ))
                (Dict.toList rawNodes)
                |> List.filterMap
                    (\( k, tp ) ->
                        case k of
                            Just key ->
                                Just ( key, tp )

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList

        indexedEdges : Dict Int Edge
        indexedEdges =
            -- Convert from a Dict keyed on position to one keyed by Int.
            Dict.toList canonicalEdges
                |> List.filterMap
                    (\( key, tps ) ->
                        case Dict.get key edgeKeysToIndex of
                            Just idx ->
                                case makeProperEdge key tps of
                                    Just edge ->
                                        Just ( idx, edge )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList

        makeProperEdge : ( LatLon, LatLon, LatLon ) -> List TrackPoint -> Maybe Edge
        makeProperEdge ( start, waypoint, end ) tps =
            let
                findNodeIndex latlon =
                    Dict.get latlon nodeCoordinatesToIndex

                ( startIdx, endIdx ) =
                    ( findNodeIndex start, findNodeIndex end )
            in
            case ( startIdx, endIdx ) of
                ( Just startNode, Just endNode ) ->
                    Just
                        { startNode = startNode
                        , endNode = endNode
                        , wayPoint = waypoint
                        , trackPoints = List.drop 1 <| List.take (List.length tps - 1) tps
                        }

                _ ->
                    Nothing

        isPointColinear : Int -> Int -> TrackPoint -> Bool
        isPointColinear startNode endNode tp =
            case ( Dict.get startNode indexedNodes, Dict.get endNode indexedNodes ) of
                ( Just start, Just end ) ->
                    trackPointSeparation start tp
                        + trackPointSeparation tp end
                        - trackPointSeparation start end
                        < 2.0

                _ ->
                    False

        singlePointLinearEdges : Dict Int Edge
        singlePointLinearEdges =
            -- Edges with one waypoint sometimes result from manually placed track points
            -- during the route planning. Removing these can result in avoiding the formation
            -- of spurious nodes either side (the extra point fools the neighbour count).
            indexedEdges
                |> Dict.filter
                    (\n e ->
                        case e.trackPoints of
                            [ tp ] ->
                                -- Exactly one point. Is it colinear with ends?
                                isPointColinear e.startNode e.endNode tp

                            _ ->
                                -- Any number other than one is of no interest
                                False
                    )

        annoyingTrackPoints =
            singlePointLinearEdges |> Dict.values |> List.concatMap .trackPoints |> List.map .idx

        removeAnnoyingPoints =
            List.filterNot
                (\tp -> List.member tp.idx annoyingTrackPoints)
                trackPoints
                |> reindexTrackpoints

        walkedRoute =
            -- Should not re-index; they should be fine.
            List.map Tuple.first walkTheRouteInternal

        reverseIndex =
            List.map2
                (\n ( _, info ) -> ( n, info ))
                (List.range 0 (List.length walkedRoute))
                walkTheRouteInternal
                |> Dict.fromList

        walkTheRouteInternal : List ( TrackPoint, PointType )
        walkTheRouteInternal =
            -- This will convert the original route into a route made from canonical edges.
            let
                addToTrail traversal accumulator =
                    let
                        getEdge =
                            Dict.get traversal.edge indexedEdges
                    in
                    case getEdge of
                        Just edge ->
                            let
                                edgeStart =
                                    Dict.get edge.startNode indexedNodes

                                edgeEnd =
                                    Dict.get edge.endNode indexedNodes
                            in
                            case ( edgeStart, edgeEnd, traversal.direction ) of
                                ( Just start, Just end, Forwards ) ->
                                    { accumulator
                                        | points =
                                            ( start, NodePoint edge.startNode )
                                                :: addEdgePoints traversal.edge edge.trackPoints
                                                ++ [ ( end, NodePoint edge.endNode ) ]
                                                ++ List.drop 1 accumulator.points
                                    }

                                ( Just start, Just end, Backwards ) ->
                                    { accumulator
                                        | points =
                                            ( end, NodePoint edge.endNode )
                                                :: (List.reverse <| addEdgePoints traversal.edge edge.trackPoints)
                                                ++ [ ( start, NodePoint edge.startNode ) ]
                                                ++ List.drop 1 accumulator.points
                                    }

                                _ ->
                                    accumulator

                        Nothing ->
                            accumulator

                addEdgePoints : Int -> List TrackPoint -> List ( TrackPoint, PointType )
                addEdgePoints edge edgePoints =
                    List.map
                        (\pt -> ( pt, EdgePoint edge pt.idx ))
                        -- Note: canonical track point index stored here.
                        edgePoints
            in
            List.foldr
                addToTrail
                { points = [], nextIdx = 0 }
                canonicalRoute
                |> .points
    in
    case annoyingTrackPoints of
        [] ->
            { empty
                | nodes = indexedNodes
                , edges = indexedEdges
                , route = canonicalRoute
                , boundingBox = box
                , trackPoints = walkedRoute
                , index = reverseIndex
            }

        _ ->
            deriveTrackPointGraph removeAnnoyingPoints box


trackPointComparable : TrackPoint -> LatLon
trackPointComparable tp =
    ( tp.lat, tp.lon )


endPoints tps =
    -- Subtle point: if the start and end coincide we want the start point to "win".
    List.take 1 (List.reverse tps) ++ List.take 1 tps


addPointsFromList listPoints dict =
    List.foldl
        (\tp d -> Dict.insert (trackPointComparable tp) tp d)
        dict
        listPoints


interestingTrackPoints : List TrackPoint -> Dict LatLon TrackPoint
interestingTrackPoints tps =
    --TODO: this should return PointOnGraph, so the end points are distinctive.
    let
        neighbourMap =
            neighbourMapHelper Dict.empty tps

        neighbourMapHelper dict tp =
            case tp of
                t0 :: t1 :: t2 :: tRest ->
                    -- "t0 and t2 are neighbours of t1"
                    neighbourMapHelper (addNeighbours dict t0 t1 t2) (t1 :: t2 :: tRest)

                [ ty, tz ] ->
                    -- "ty is the only neighbour for tz"
                    addOneNeighbourAtEnd dict ty tz

                _ ->
                    dict

        addNeighbours dict t0 t1 t2 =
            -- "t0 and t2 are neighbours of t1"
            let
                key =
                    trackPointComparable t1

                current =
                    Dict.get key dict

                ( left, right ) =
                    ( trackPointComparable t0, trackPointComparable t2 )
            in
            Dict.insert key
                (case current of
                    Just ( tp, neighbours ) ->
                        -- Use whichever track point we first saw at this location.
                        ( tp, Set.insert left <| Set.insert right neighbours )

                    Nothing ->
                        -- t1 becomes the examplar track point at this location.
                        ( t1, Set.insert left <| Set.insert right Set.empty )
                )
                dict

        addOneNeighbourAtEnd dict ty tz =
            let
                key =
                    trackPointComparable tz

                current =
                    Dict.get key dict

                left =
                    trackPointComparable ty
            in
            Dict.insert key
                (case current of
                    Just ( tp, neighbours ) ->
                        ( tp, Set.insert left neighbours )

                    Nothing ->
                        ( tz, Set.insert left Set.empty )
                )
                dict

        notTwoNeighbours _ ( _, neighbours ) =
            -- Three or more is a junction.
            -- Need to add S/F explicitly
            Set.size neighbours > 2

        interesting =
            Dict.filter notTwoNeighbours neighbourMap
    in
    -- We don't need to return the neighbour list.
    interesting
        |> Dict.map (\k ( tp, _ ) -> tp)
        |> addPointsFromList (endPoints tps)


findDistinctEdges :
    Dict LatLon TrackPoint
    -> List TrackPoint
    -> List (List TrackPoint)
findDistinctEdges nodes trackPoints =
    -- I probably should develop this into what I really want, but it's quite neat
    -- and clear, so I think I'll make another pass to substitute the canonical edges.
    let
        atNode : TrackPoint -> Bool
        atNode tp =
            Dict.member (trackPointComparable tp) nodes

        routeSplitter :
            TrackPoint -- The start point of an edge == a junction or the route start
            -> List (List TrackPoint) -- Accumulator for fold.
            -> List TrackPoint -- The list being folded.
            -> List (List TrackPoint) -- The result list, in the natural order.
        routeSplitter startNode edges tps =
            -- Not quite what we want as the node to appear on both edges; here it's on the departing edge.
            let
                split =
                    -- Split before next node.
                    List.splitWhen atNode tps
            in
            case split of
                Just ( before, after0 :: after ) ->
                    -- We borrow the first node of the next edge here for our edge, and pass it forwards.
                    routeSplitter
                        after0
                        ((startNode :: before ++ [ after0 ]) :: edges)
                        after

                Just ( before, _ ) ->
                    -- Last edge, so just prepend the carried forward node.
                    (startNode :: before) :: edges

                Nothing ->
                    -- Reverse list so it's in the natural order.
                    edges |> List.reverse

        edgeList : List (List TrackPoint)
        edgeList =
            case trackPoints of
                p0 :: ps ->
                    routeSplitter p0 [] trackPoints

                _ ->
                    []
    in
    -- First edge (as coded) will just contain the start node.
    List.drop 1 edgeList


findCanonicalEdges :
    List (List TrackPoint)
    -> Dict ( LatLon, LatLon, LatLon ) (List TrackPoint)
findCanonicalEdges originalEdges =
    -- Note we are keying on three coordinates, so we disambiguate edges between node pairs.
    -- I am now thinking of making two entries, one for each direction.
    -- Marginally larger dict, much easier lookup.
    let
        addCanonical :
            List TrackPoint
            -> Dict ( LatLon, LatLon, LatLon ) (List TrackPoint)
            -> Dict ( LatLon, LatLon, LatLon ) (List TrackPoint)
        addCanonical edge dict =
            let
                startNode =
                    List.head edge

                secondNode =
                    List.head <| List.drop 1 edge

                backwardsEdge =
                    List.reverse edge

                finishNode =
                    List.head backwardsEdge

                penultimateNode =
                    List.head <| List.drop 1 backwardsEdge
            in
            case [ startNode, secondNode, penultimateNode, finishNode ] of
                [ Just start, Just second, Just penultimate, Just finish ] ->
                    let
                        comp1 =
                            trackPointComparable start

                        comp2 =
                            trackPointComparable second

                        compM =
                            trackPointComparable penultimate

                        compN =
                            trackPointComparable finish
                    in
                    if
                        -- We may have encountered in either direction.
                        Dict.member ( comp1, comp2, compN ) dict
                            || Dict.member ( compN, compM, comp1 ) dict
                    then
                        -- Previously encountered.
                        dict

                    else
                        -- First encounter for this edge, so this is canonical.
                        Dict.insert ( comp1, comp2, compN )
                            edge
                            dict

                _ ->
                    dict
    in
    List.foldl addCanonical Dict.empty originalEdges


useCanonicalEdges :
    List (List TrackPoint)
    -> Dict ( LatLon, LatLon, LatLon ) (List TrackPoint)
    -> List ( ( LatLon, LatLon, LatLon ), Direction )
useCanonicalEdges edges canonicalEdges =
    let
        replaceEdge : List TrackPoint -> Maybe ( ( LatLon, LatLon, LatLon ), Direction )
        replaceEdge edge =
            let
                startNode =
                    List.head edge

                secondNode =
                    List.head <| List.drop 1 edge

                backwardsEdge =
                    List.reverse edge

                finishNode =
                    List.head backwardsEdge

                penultimateNode =
                    List.head <| List.drop 1 backwardsEdge
            in
            case [ startNode, secondNode, penultimateNode, finishNode ] of
                [ Just start, Just second, Just penultimate, Just finish ] ->
                    let
                        comp1 =
                            trackPointComparable start

                        comp2 =
                            trackPointComparable second

                        compM =
                            trackPointComparable penultimate

                        compN =
                            trackPointComparable finish
                    in
                    if Dict.member ( comp1, comp2, compN ) canonicalEdges then
                        Just ( ( comp1, comp2, compN ), Forwards )

                    else if Dict.member ( compN, compM, comp1 ) canonicalEdges then
                        Just ( ( compN, compM, comp1 ), Backwards )

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.map replaceEdge edges |> List.filterMap identity


walkTheRoute : Graph -> List TrackPoint
walkTheRoute graph =
    let
        _ =
            Debug.log "Route " <| List.map .idx route

        route =
            graph.trackPoints
                |> List.map (applyCentreLineOffset graph.centreLineOffset)
                |> reindexTrackpoints
    in
    route


applyCentreLineOffset : Float -> TrackPoint -> TrackPoint
applyCentreLineOffset offset trackpoint =
    let
        offsetDirection =
            trackpoint.naturalBearing
                |> Angle.radians
                |> Direction3d.yx
                |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

        offsetVector =
            Vector3d.withLength (Length.meters offset) offsetDirection

        newXYZ =
            Point3d.translateBy offsetVector trackpoint.xyz

        ( lon, lat, ele ) =
            toGPXcoords newXYZ
    in
    { trackpoint
        | lat = lat
        , lon = lon
        , xyz = newXYZ
    }


nodePointList : Graph -> List (Point3d Length.Meters LocalCoords)
nodePointList graph =
    -- Have to shift according to the track point box
    let
        ( midLon, midLat, _ ) =
            Point3d.toTuple Length.inMeters <| BoundingBox3d.centerPoint graph.boundingBox

        midXYZ =
            fromGPXcoords midLon midLat 0

        shift =
            Vector3d.from midXYZ Point3d.origin

        whereTheNodesAre =
            graph.nodes
                |> Dict.values
                |> List.map (.xyz >> Point3d.translateBy shift)
    in
    whereTheNodesAre


withinSameEdge : Graph -> ( Int, Int ) -> Maybe ( Int, Int )
withinSameEdge graph ( tp1, tp2 ) =
    -- Is editing possible -- are these trackpoints on same edge?
    -- Iff they are, return their canonical equivalents.
    if graph == empty then
        Just ( tp1, tp2 )

    else
        let
            ( point1, point2 ) =
                ( Dict.get tp1 graph.index, Dict.get tp2 graph.index )
        in
        case ( point1, point2 ) of
            ( Just (EdgePoint e1 canon1), Just (EdgePoint e2 canon2) ) ->
                if e1 == e2 then
                    Just ( min canon1 canon2, max canon1 canon2 )

                else
                    Nothing

            _ ->
                Nothing
