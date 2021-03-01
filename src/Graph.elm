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
import Array exposing (Array)
import Axis3d
import Dict exposing (Dict)
import Direction3d
import Element as E exposing (Element, alignTop, padding, spacing)
import Element.Input as I
import Length
import List.Extra as List
import Point3d
import Set exposing (Set)
import TrackPoint exposing (TrackPoint, TrackPointType(..), reindexTrackpoints, toGPXcoords)
import Utils exposing (showDecimal2)
import Vector3d
import ViewPureStyles exposing (prettyButtonStyles)


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
    }


empty =
    { nodes = Dict.empty, edges = Dict.empty, route = [], centreLineOffset = 0.0 }


type Msg
    = GraphAnalyse
    | CentreLineOffset Float


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


viewGraphControls : Graph -> (Msg -> msg) -> Element msg
viewGraphControls graph wrapper =
    E.row [ spacing 10, padding 10 ]
        [ I.slider
            []
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
        , I.button prettyButtonStyles
            { onPress = Just (wrapper GraphAnalyse)
            , label = E.text "Analyse"
            }
        ]


update msg model =
    case msg of
        GraphAnalyse ->
            deriveTrackPointGraph model.trackPoints

        CentreLineOffset offset ->
            let
                newGraph g =
                    { g | centreLineOffset = offset }
            in
            newGraph model.graph


deriveTrackPointGraph : List TrackPoint -> Graph
deriveTrackPointGraph trackPoints =
    let
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
            Dict.fromList <|
                List.map2
                    (\key idx -> ( key, idx ))
                    (Dict.keys rawNodes)
                    (List.range 1 (Dict.size rawNodes))

        edgeKeysToIndex : Dict ( LatLon, LatLon, LatLon ) Int
        edgeKeysToIndex =
            Dict.fromList <|
                List.map2
                    (\key idx -> ( key, idx ))
                    (Dict.keys canonicalEdges)
                    (List.range 1 (Dict.size canonicalEdges))

        indexedNodes : Dict Int TrackPoint
        indexedNodes =
            List.map
                (\( k, v ) -> ( Dict.get k nodeCoordinatesToIndex, v ))
                (Dict.toList rawNodes)
                |> List.filterMap
                    (\( k, tp ) ->
                        case k of
                            Just key ->
                                Just ( key, { tp | info = NodePoint key } )

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList

        indexedEdges : Dict Int Edge
        indexedEdges =
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
                        , trackPoints = List.drop 1 <| List.take (List.length trackPoints - 1) tps
                        }

                _ ->
                    Nothing
    in
    { nodes = indexedNodes
    , edges = indexedEdges
    , route = canonicalRoute
    , centreLineOffset = 0.0
    }


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


traversalAsTrackPoints : Graph -> Traversal -> List TrackPoint
traversalAsTrackPoints graph traversal =
    let
        getEdge =
            Dict.get traversal.edge graph.edges
    in
    case getEdge of
        Just edge ->
            let
                edgeStart =
                    Dict.get edge.startNode graph.nodes

                edgeEnd =
                    Dict.get edge.endNode graph.nodes
            in
            case ( edgeStart, edgeEnd, traversal.direction ) of
                ( Just start, Just end, Forwards ) ->
                    start
                        :: List.map2
                            (\point n -> { point | info = EdgePoint traversal.edge n })
                            edge.trackPoints
                            (List.range 1 (List.length edge.trackPoints))
                        ++ [ end ]

                ( Just start, Just end, Backwards ) ->
                    end
                        :: List.map2
                            (\point n -> { point | info = EdgePoint traversal.edge n })
                            (List.reverse edge.trackPoints)
                            (List.range 1 (List.length edge.trackPoints))
                        ++ [ start ]

                _ ->
                    []

        Nothing ->
            []


walkTheRoute : Graph -> List TrackPoint
walkTheRoute graph =
    -- This will convert the original route into a route made from canonical edges.
    let
        addToTrail traversal accumulator =
            let
                getEdge =
                    Dict.get traversal.edge graph.edges
            in
            case getEdge of
                Just edge ->
                    let
                        edgeStart =
                            Dict.get edge.startNode graph.nodes

                        edgeEnd =
                            Dict.get edge.endNode graph.nodes
                    in
                    case ( edgeStart, edgeEnd, traversal.direction ) of
                        ( Just start, Just end, Forwards ) ->
                            { accumulator
                                | points =
                                    start
                                        :: addEdgePoints traversal.edge edge.trackPoints
                                        ++ [ end ]
                                        ++ List.drop 1 accumulator.points
                            }

                        ( Just start, Just end, Backwards ) ->
                            { accumulator
                                | points =
                                    end
                                        :: (List.reverse <| addEdgePoints traversal.edge edge.trackPoints)
                                        ++ [ start ]
                                        ++ List.drop 1 accumulator.points
                            }

                        _ ->
                            accumulator

                Nothing ->
                    accumulator

        addEdgePoints : Int -> List TrackPoint -> List TrackPoint
        addEdgePoints edge edgePoints =
            List.map2
                (\point n -> { point | info = EdgePoint edge n })
                edgePoints
                (List.range 1 (List.length edgePoints))
    in
    List.foldr
        addToTrail
        { points = [], nextIdx = 0 }
        graph.route
        |> .points
        |> reindexTrackpoints
        |> List.map (applyCentreLineOffset graph.centreLineOffset)


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


makeSimpleGraph : List TrackPoint -> Graph
makeSimpleGraph trackPoints =
    -- Use this when file is loaded, so we always have a graph.
    -- Edge ID = 1, node IDs - 0, 1.
    let
        startTP =
            List.head trackPoints

        endTP =
            List.last trackPoints

        wayPointTP =
            List.head <| List.drop 1 trackPoints

        otherTP =
            List.drop 1 <| List.take (List.length trackPoints - 1) trackPoints

        addNodeInfo info tp =
            { tp | info = info }

        otherNodes =
            List.map2
                (\tp i -> addNodeInfo (EdgePoint 1 i) tp)
                otherTP
                (List.range 1 (List.length otherTP))
    in
    case ( startTP, wayPointTP, endTP ) of
        ( Just start, Just end, Just way ) ->
            let
                startNode =
                    addNodeInfo (StartPoint 0) start

                endNode =
                    addNodeInfo (EndPoint 1) end

                nodeDict =
                    Dict.empty
                        |> Dict.insert 0 startNode
                        |> Dict.insert 1 endNode

                edge =
                    { startNode = 0
                    , endNode = 1
                    , wayPoint = trackPointComparable way
                    , trackPoints = otherNodes
                    }

                edgeDict =
                    Dict.insert 1 edge Dict.empty

                traversal =
                    { edge = 1, direction = Forwards }
            in
            { nodes = nodeDict
            , edges = edgeDict
            , route = [ traversal ]
            , centreLineOffset = 0.0
            }

        _ ->
            empty


onSameEdge : Array TrackPoint -> Int -> Int -> Bool
onSameEdge array p1 p2 =
    -- Allows editing tools to make sure user is not trying something that won't work.
    -- And for the editing tools not to make the same mistake.
    -- SHOULD THEY BE ON THE SAME TRAVERSAL ???
    let
        ( node1, node2 ) =
            ( Array.get p1 array, Array.get p2 array )

        ( info1, info2 ) =
            ( Maybe.map .info node1, Maybe.map .info node2 )
    in
    case ( info1, info2 ) of
        ( Just (EdgePoint e1 _), Just (EdgePoint e2 _) ) ->
            e1 == e2

        _ ->
            False


showNodeInfo node =
    case node.info of
        StartPoint n ->
            E.column [ alignTop ]
                [ E.text <| "Start: " ++ String.fromInt n ]

        EdgePoint e n ->
            E.column [ alignTop ]
                [ E.text <| "Edge: " ++ String.fromInt e
                , E.text <| "Offset: " ++ String.fromInt n
                ]

        EndPoint n ->
            E.column [ alignTop ]
                [ E.text <| "End: " ++ String.fromInt n ]

        NodePoint n ->
            E.column [ alignTop ]
                [ E.text <| "Node: " ++ String.fromInt n ]

        _ ->
            E.column [ alignTop ]
                [ E.text "Mystery point" ]
