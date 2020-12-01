module Terrain2 exposing (makeTerrain2)

import Angle exposing (Angle)
import Color
import Geometry101 exposing (angleBetweenVectors, distance)
import Length exposing (Length, Meters)
import NodesAndRoads exposing (DrawingNode, DrawingRoad, GroundCoords, roadAsLine, roadAsVector, vectorFromTwoNodes)
import Point3d exposing (Point3d)
import RenderingContext exposing (RenderingContext)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Triangle3d

type alias RoadRelativeInfo =
    {
      relativeBearingToStart : Float
    , bearingOfRoad : Float
    , distance : Length
    , fromPoint : DrawingRoad
    , toPoint : DrawingRoad
    }

type NodeType
    = RoadStart
    | RoadEnd

type RoadPresentation
    -- Which side of another road segment can we see?
    -- How we treat it depends which side we are looking.
    -- Essentially this is because we build triangles separately each side to
    -- remove any risks of road overlap.
    -- We will (probably) ignore the "exactly aligned" case because if the two
    -- ends are very close, the sweep algorithm will just take care of it.
    = LeftSide
    | RightSide

type alias NodeRelativeInfo =
    { nodeType : NodeType
    , relativeBearing : Float
    , bearingOfRoad : Float
    , distance : Length
    , fromPoint : DrawingRoad
    , toPoint : DrawingRoad
    }


{-
   Walk the track, paying attention to those nodes on the right
   that are not obscured by other nodes. Note we can only "see"
   a road (and its nodes) if it presents its Right side to us;
   Left sides merely obscure nodes that would have been visible.

   Several operations:
   1.  Find the bearing, relative to current road, of all subsequent nodes.
   2.  Separate into Left and Right nodes, sorted by increasing absolute bearing.

   Treating each side separately:
   3.  Use a "scan" algorithm by increasing (absolute) bearing, to find visible nodes.
       This accounts for the "sideness" of road segments; segments in wrong direction obscure.

   Once we know, all visible nodes, we start to work out triangles:
   4.  Other than first node, we have "carried over" the knowledge of which node has been claimed
       by the road segment prior to our current one.
   5.  Proceeding again in bearing order (which is a subset of rute order, I think,
       because of route continuity), form a triangle with visible node and our segment.
   6.  Form comparable triangle from the next segment (if any).
   7.  Determine perimeters of each triangle.
   8.  If the perimeter of "our" triangle is less than that of the next, then we "claim"
       that node. This becomes an output triangle.
   9.  If our perimeter is more, then we can (probably) stop.
   10. We recurse to the next node _along the route_, passing forward the last claimed node.

   When this is done, we may have some gaps, though I think not.
   We will need to attend to the "outside", so that triangles are formed with the boundary (corners?)
   of the geographic space.
-}

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


makeTerrain2 :
    RenderingContext
    -> List DrawingNode
    -> List (Entity GroundCoords)
makeTerrain2 context roads =
    let
        faces =
            []

        drawFace face =
            let
                ( v1, v2, v3 ) =
                    -- Would be obviated by using geometry consistently!
                    face.vertices
            in
            Scene3d.facet
                (Material.matte <| Color.darkGreen)
            <|
                Triangle3d.from v1 v2 v3
    in
    List.map drawFace faces
