module Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.
-- I'm going to migrate current functionality to this before creating the editing suite.

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
    { nodes : Dict LatLon Node
    , edges : List (List TrackPoint) -- Edge
    }


empty =
    { nodes = Dict.empty, edges = [] }


type Msg
    = GraphAnalyse


type alias Node =
    TrackPoint


type alias Edge =
    { start : Node
    , finish : Node
    , firstStep : Node -- disambiguate if there are two edges between a node pair.
    , trackPoints : List TrackPoint
    }


type alias EdgeTraversal =
    { edge : Edge
    , direction : Direction
    , centrelineOffset : Float -- metres right (-ve == left) of centreline.
    }


type alias Route =
    { route : List EdgeTraversal }


viewGraphControls : (Msg -> msg) -> Element msg
viewGraphControls wrapper =
    I.button prettyButtonStyles
        { onPress = Just (wrapper GraphAnalyse)
        , label = E.text "Analyse"
        }


update msg model =
    let
        _ =
            Debug.log "Look, nodes!"
                (nodes |> Dict.toList |> List.map (\( k, v ) -> v.idx) |> List.sort)

        _ =
            Debug.log "Look, edges!"
                (edges |> List.map (List.map .idx))

        nodes =
            interestingTrackPoints model.trackPoints

        edges =
            findDistinctEdgesAndFirstTraversal nodes model.trackPoints
    in
    { model | graph = { nodes = nodes, edges = edges } }


trackPointComparable : TrackPoint -> LatLon
trackPointComparable tp =
    ( tp.lat, tp.lon )


interestingTrackPoints : List TrackPoint -> Dict LatLon TrackPoint
interestingTrackPoints tps =
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


findDistinctEdgesAndFirstTraversal :
    Dict LatLon TrackPoint
    -> List TrackPoint
    -> List (List TrackPoint)



---> Dict ( LatLon, LatLon ) (List ( LatLon, List TrackPoint ))


findDistinctEdgesAndFirstTraversal nodes trackPoints =
    -- Possible edge dict is keyed by two latlon pairs, but with the convention that the SouthWest
    -- node is listed first (so we can traverse both ways). The latlon in the value tuple is the
    -- nearest track point to the SW end, to disambiguate two distinct edges between a node pair.
    -- (Mostly the value will be a singleton list.)
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

        addTrailingNode : List TrackPoint -> List TrackPoint -> List TrackPoint
        addTrailingNode edge1 edge2 =
            edge1 ++ List.take 1 edge2

        edgeList : List (List TrackPoint)
        edgeList =
            case trackPoints of
                p0 :: ps ->
                    routeSplitter p0 [] trackPoints

                _ ->
                    []
    in
    edgeList
