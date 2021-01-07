module ScenePainter exposing (..)

-- This is a very thin wrapper around 3d scene so I can switch between unlit and sunny.

import Direction3d exposing (negativeZ, positiveZ)
import Scene3d


render3dScene withLighting sceneProps =
    if withLighting then
        Scene3d.sunny
            { camera = sceneProps.camera
            , dimensions = sceneProps.dimensions
            , background = sceneProps.background
            , clipDepth = sceneProps.clipDepth
            , entities = sceneProps.entities
            , upDirection = positiveZ
            , sunlightDirection = negativeZ
            , shadows = False
            }

    else
        Scene3d.unlit sceneProps
