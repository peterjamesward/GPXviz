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

import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Input as I
import List.Extra as List
import Set exposing (Set)
import TrackPoint exposing (TrackPoint, TrackPointType(..))
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
    }


type PointType
    = StartPoint Int
    | FinishPoint Int
    | JunctionPoint Int
    | OtherPoint Int Int -- Store both the edge number and the unique number (which might be locally unique).


type alias PointOnGraph =
    -- This will become our new "enhanced" track point.
    { lat : Float
    , lon : Float
    , ele : Float
    , idx : Int
    , info : PointType -- we know the point's context and behaviours.
    }


empty =
    { nodes = Dict.empty, edges = Dict.empty, route = [] }


type Msg
    = GraphAnalyse


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


viewGraphControls : (Msg -> msg) -> Element msg
viewGraphControls wrapper =
    I.button prettyButtonStyles
        { onPress = Just (wrapper GraphAnalyse)
        , label = E.text "Analyse"
        }


update msg model =
    case msg of
        GraphAnalyse ->
            deriveTrackPointGraph model.trackPoints


deriveTrackPointGraph : List TrackPoint -> Graph
deriveTrackPointGraph trackPoints =
    let
        _ =
            Debug.log "The nodes are "
                indexedNodes

        _ =
            Debug.log "The route is "
                canonicalRoute

        rawNodes =
            interestingTrackPoints trackPoints

        rawEdges =
            findDistinctEdges rawNodes trackPoints

        canonicalEdges =
            findCanonicalEdges rawNodes rawEdges

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
    { nodes = indexedNodes, edges = indexedEdges, route = canonicalRoute }


trackPointComparable : TrackPoint -> LatLon
trackPointComparable tp =
    ( tp.lat, tp.lon )


interestingTrackPoints : List TrackPoint -> Dict LatLon TrackPoint
interestingTrackPoints tps =
    --TODO: this should return PointOnGraph, so the end points are distinctive.
    let
        endPoints =
            -- Subtle point: if the start and end coincide we want the start point to "win".
            List.take 1 (List.reverse tps) ++ List.take 1 tps

        addPointsFromList listPoints dict =
            List.foldl
                (\tp d -> Dict.insert (trackPointComparable tp) tp d)
                dict
                listPoints

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
        |> addPointsFromList endPoints


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
            TrackPoint
            -> List (List TrackPoint)
            -> List TrackPoint
            -> List (List TrackPoint)
        routeSplitter startNode edges tps =
            -- Not quite what we want as the node to appear on both edges; here it's on the departing edge.
            let
                split =
                    -- Split before next node.
                    List.splitWhen atNode tps
            in
            case split of
                Just ( before, after0 :: after ) ->
                    routeSplitter after0 ((startNode :: before ++ [ after0 ]) :: edges) after

                Just ( before, _ ) ->
                    (startNode :: before) :: edges

                Nothing ->
                    edges |> List.reverse

        edgeList : List (List TrackPoint)
        edgeList =
            case trackPoints of
                p0 :: ps ->
                    routeSplitter p0 [] trackPoints

                _ ->
                    []
    in
    List.drop 1 edgeList


findCanonicalEdges :
    Dict LatLon TrackPoint
    -> List (List TrackPoint)
    -> Dict ( LatLon, LatLon, LatLon ) (List TrackPoint)
findCanonicalEdges nodes originalEdges =
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
                        Dict.member ( comp1, comp2, compN ) dict
                            || Dict.member ( compN, compM, comp1 ) dict
                    then
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
    -- This will convert the original route into a route made from canonical edges.
    -- Let us put it to test.
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
