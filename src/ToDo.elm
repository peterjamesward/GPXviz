module ToDo exposing (..)

--TODO: Priority. Make so we can nudge or delete the last TP. This should involve going back to TP basics (roads are merely visual artefacts). Might be able to get rid of many Maybe tests in the process.

--TODO: Show yellow smoothed bend option on map. and nudge line.

--TODO: Set & Show current and marked node on map.
--      (Click to set Orange position, Shift-click for Purple?)

--TODO: Result of above, editing should work on map.

--TODO: Direct editing on map (may be easier than in elm-3d-scene).

--TODO: Re-grouping of controls (see Jason Hurst input).

--TODO: SVG overlay - like an HUD? (elm example available)

--TODO: Scale of some kind on graphics. (Probably o nmap already.)

--TODO: Feat? Clicking & Dragging on Plan & Elevation views.
--Ian MacKenzie (himself) says Camera3d.ray will be useful here!

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!
--HOLD: Feat. Say if filename is too long for Magic Roads. (50 chars.)
--DROP: Scroll wheel zoom? (Quick check in Elm shows this is really nasty JS + ports solution).

