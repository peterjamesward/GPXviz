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
import TrackPoint exposing (TrackPoint)
import ViewPureStyles exposing (prettyButtonStyles)


type Direction
    = Forwards
    | Backwards


type alias LatLon =
    ( Float, Float )


type alias Graph =
    { nodes : Dict LatLon TrackPoint -- I want this to be Dict Int Node
    , edges : List (List TrackPoint) -- and Dict (Int, Int, LatLon) Edge
    , route : List Traversal -- this is OK.
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
    { nodes = Dict.empty, edges = [], route = [] }


type Msg
    = GraphAnalyse


type alias Node =
    TrackPoint


type alias Edge =
    List TrackPoint


type alias Traversal =
    { edge : Edge
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


deriveTrackPointGraph trackPoints =
    let
        --_ =
        --    Debug.log "Look, nodes!"
        --        (rawNodes |> Dict.toList |> List.map (\( k, v ) -> v.idx) |> List.sort)
        --
        --_ =
        --    Debug.log "Look, edges!"
        --        (rawEdges |> List.map (List.map .idx))
        --
        --_ =
        --    Debug.log "Canonical edges"
        --        (canonicalEdges
        --            |> Dict.keys
        --            |> List.map (\( n1, _, n3 ) -> ( nodeIdx n1, nodeIdx n3 ))
        --        )
        --
        --_ =
        --    Debug.log "Canonical route" <|
        --        List.filterMap identity <|
        --            List.map
        --                (\( n1, n2, n3 ) ->
        --                    let
        --                        ( mstart, mfinish ) =
        --                            ( Dict.get n1 rawNodes, Dict.get n3 rawNodes )
        --
        --                        mtraversal =
        --                            Dict.get ( n1, n2, n3 ) canonicalEdges
        --                    in
        --                    case ( mstart, mfinish, mtraversal ) of
        --                        ( Just start, Just finish, Just traversal ) ->
        --                            Just ( traversal.direction, start.idx, finish.idx )
        --
        --                        _ ->
        --                            Nothing
        --                )
        --                canonicalRoute
        nodeIdx latLon =
            case Dict.get latLon rawNodes of
                Just tp ->
                    tp.idx

                Nothing ->
                    -1

        rawNodes =
            interestingTrackPoints trackPoints

        rawEdges =
            findDistinctEdges rawNodes trackPoints

        canonicalEdges =
            findCanonicalEdges rawNodes rawEdges

        canonicalRoute : List ( LatLon, LatLon, LatLon )
        canonicalRoute =
            useCanonicalEdges rawEdges canonicalEdges

        reindexNodeDict : LatLon -> TrackPoint -> Dict Int PointOnGraph -> Dict Int PointOnGraph
        reindexNodeDict latLon tp dict =
            -- Final pass, in which we normalise our indexing.
            Dict.insert tp.idx
                { lat = tp.lat
                , lon = tp.lon
                , ele = tp.ele
                , idx = tp.idx
                , info =
                    case tp.idx of
                        0 ->
                            StartPoint 0

                        _ ->
                            JunctionPoint tp.idx
                }
                dict

        finalNodeDict : Dict LatLon TrackPoint -> Dict Int PointOnGraph
        finalNodeDict =
            -- Rebuild the node dict keyed by the original trackpoint ID (because it's convenient).
            Dict.foldl
                reindexNodeDict
                Dict.empty

        packageTheEdge : List TrackPoint -> Dict Int Edge -> Dict Int Edge
        packageTheEdge tps dict =
            let
                edgeStartNode =
                    List.head tps

                edgeEndNode =
                    List.last tps

                otherNodes =
                    List.take (List.length tps - 1) tps |> List.drop 1
            in
            Dict.empty

        finalEdgeDict : List (List TrackPoint) -> Dict Int Edge
        finalEdgeDict listOfLostOfTP =
            List.foldl packageTheEdge Dict.empty listOfLostOfTP
    in
    { nodes = rawNodes, edges = rawEdges, route = [] }


trackPointComparable : TrackPoint -> LatLon
trackPointComparable tp =
    ( tp.lat, tp.lon )


interestingTrackPoints : List TrackPoint -> Dict LatLon TrackPoint
interestingTrackPoints tps =
    --TODO: this should return PointOnGraph, so the end points are distinctive.
    let
        endPoints =
            -- Subtle point: if the start and end co-incide we want the start point to "win".
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
            -- One neighbour is an end; three or more is a junction.
            Set.size neighbours /= 2

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
    -> Dict ( LatLon, LatLon, LatLon ) Traversal
findCanonicalEdges nodes originalEdges =
    -- Note we are keying on three coordinates, so we disambiguate edges between node pairs.
    -- I am now thinking of making two entries, one for each direction.
    -- Marginally larger dict, much easier lookup.
    let
        addCanonical :
            List TrackPoint
            -> Dict ( LatLon, LatLon, LatLon ) Traversal
            -> Dict ( LatLon, LatLon, LatLon ) Traversal
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
                        dict
                            |> Dict.insert ( comp1, comp2, compN )
                                { edge = edge
                                , direction = Forwards
                                }
                            |> Dict.insert ( compN, compM, comp1 )
                                { edge = edge
                                , direction = Backwards
                                }

                _ ->
                    dict
    in
    List.foldl addCanonical Dict.empty originalEdges


useCanonicalEdges :
    List (List TrackPoint)
    -> Dict ( LatLon, LatLon, LatLon ) Traversal
    -> List ( LatLon, LatLon, LatLon )
useCanonicalEdges edges canonicalEdges =
    -- Don't do this, really. Just return the Route, then walk the route.
    let
        replaceEdge : List TrackPoint -> Maybe ( LatLon, LatLon, LatLon )
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
                        Just ( comp1, comp2, compN )

                    else if Dict.member ( compN, compM, comp1 ) canonicalEdges then
                        Just ( compN, compM, comp1 )

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.map replaceEdge edges |> List.filterMap identity
