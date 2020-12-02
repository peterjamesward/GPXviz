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
