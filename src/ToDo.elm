module ToDo exposing (..)

--IDEA: Map on sea level plane.

{- See
        https://earth-info.nga.mil/GandG/wgs84/web_mercator/(U)%20NGA_SIG_0011_1.0.0_WEBMERC.pdf
-}

-- Not possible to map an image to elm-3d-scene. Could use transparent background but
-- then care needed to synchronise map zoom and position. Doable but is it worth it?

--TODO: Feat? Clicking & Dragging on Plan & Elevation views.
--Ian MacKenzie (himself) says Camera3d.ray will be useful here!

--TODO: Feat. Say if filename is too long for Magic Roads. (50 chars.)

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Report input file parse errors.