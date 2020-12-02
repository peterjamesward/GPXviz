module Stash exposing (..)



sortByRelativeBearing : DrawingRoad -> List DrawingRoad -> (List DrawingRoad, List DrawingRoad)
sortByRelativeBearing current remaining =
    let
        -- We need list of *nodes* sorted by alpha. When we encounter a start
        -- node, we add to visible, if it presents the correct side, and we add to the occlusion list;
        -- with an end node, it is also visible depending on presentation, and remove from occlusion list.
        -- Actually, simple-ish.
        -- Also, past roads can also obscure, surely.
        nodesWithRelativeInfo : List NodeRelativeInfo
        nodesWithRelativeInfo =
            List.concatMap (nodeRelativeInfo) remaining


        nodeRelativeInfo : DrawingRoad -> List NodeRelativeInfo
        nodeRelativeInfo other =
         [   { nodeType = RoadStart
            , relativeBearingToStart = relativeBearing current other.startsAt
            , bearingOfRoad = other.bearing
            , distance = distance current.startsAt other.start
            , fromPoint = current
            , toPoint = other
            }
         ,   { nodeType = RoadEnd
            , relativeBearingToStart = relativeBearing current other.endsAt
            , bearingOfRoad = other.bearing
            , distance = distance current.startsAt other.endsAt
            , fromPoint = current
            , toPoint = other
            }
            ]

        relativeBearing : DrawingRoad -> DrawingNode -> Float
        relativeBearing fromRoad toNode =
            angleBetweenVectors
                (roadAsVector fromRoad)
                (vectorFromTwoNodes fromRoad.startsAt toNode)

        (leftList, rightList) =
            List.partition
                (.relativeBearing >> isLeft)
                nodesWithRelativeInfo

        (leftSortedByBearing, rightSortedByBearing) =
            -- If clockwise rotation is positive
            ( List.sortBy (negate << .relativeBearing) leftList
            , List.sortBy .relativeBearing rightList
            )

        isLeft a =
            a < 0.0

        isRight a =
            a > 0.0

        roadPresentation : DrawingRoad -> NodeRelativeInfo -> RoadPresentation
        roadPresentation thisRoad viewedNode =
            let
                (ourVector, itsVector) =
                    ( roadAsVector thisRoad
                    , roadAsVector viewedNode.toPoint
                    )

                relativeDirection =
                    (angleBetweenVectors ourVector itsVector)
            in
                case compare relativeDirection 0.0  of
                    LT -> LeftSide
                    EQ -> LeftSide -- doesn't matter, I hope.
                    GT -> RightSide


        processNode : DrawingNode
           -> (List NodeRelativeInfo, List DrawingRoad)
           -> (List NodeRelativeInfo, List DrawingRoad)
        processNode node (occluded, visible) =
            -- Here we act on the nodes in angle order and decide whether they
            -- add to the visible or occlusion lists. Keep in mind we are turning leftwards from
            -- our local centre-line. Let's enumerate the possibilities:
            -- 1. It's a Start node and the road presents its left side to us.
            -- 2. It's a Start node and the road presents its right side to us.
            -- 3. The road is aligned along our line of sight.
            -- 4. It's an End node and the road presents its left side.
            -- 5. It's an End node and the road presents its right side.
            case (node.nodeType, roadPresentation current node) of
                (RoadStart, LeftSide) ->
                (RoadStart, RightSide) ->

                (RoadEnd, LeftSide) ->
                (RoadEnd, RightSide) ->

        leftVisibleRoads =
            let
                initialOcclusionList = []
                initialVisibleList = []
            in
            List.foldl processNode (initialOcclusionList, initialVisibleList) leftSortedByBearing
