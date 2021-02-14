module Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.
-- I'm going to migrate current functionality to this before creating the editing suite.

import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Input as I
import Set
import TrackPoint exposing (TrackPoint)
import ViewPureStyles exposing (prettyButtonStyles)


type Direction
    = Forwards
    | Backwards


type alias Graph =
    { nodes : Dict ( Float, Float ) Node
    , edges : List Edge
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
            Debug.log "Look, nodes!" nodes

        nodes =
            interestingTrackPoints model.trackPoints
    in
    { model | graph = { nodes = nodes, edges = [] } }


interestingTrackPoints tps =
    let
        trackPointComparable : TrackPoint -> ( Float, Float )
        trackPointComparable tp =
            ( tp.lat, tp.lon )

        neighbourMap =
            tps |> neighbourMapHelper Dict.empty

        neighbourMapHelper dict tp =
            case tp of
                t0 :: t1 :: t2 :: tRest ->
                    neighbourMapHelper (addNeighbours dict t0 t1 t2) (t1 :: t2 :: tRest)

                _ ->
                    dict

        addNeighbours dict t0 t1 t2 =
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
                        ( tp, Set.insert left <| Set.insert right neighbours )

                    Nothing ->
                        ( t1, Set.insert left <| Set.insert right Set.empty )
                )
                dict

        threeDistinct _ ( _, neighbours ) =
            Set.size neighbours >= 3

        interesting =
            Dict.filter threeDistinct neighbourMap
    in
    interesting |> Dict.map (\k ( tp, _ ) -> tp)
