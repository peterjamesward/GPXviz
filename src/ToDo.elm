module ToDo exposing (..)

--TODO: BUG. Set & Show current and marked node on map.
 -- Dodgy protocol. Ugly. Everything needs to go through MapInfo?
 -- Pointer not visible when switching out of map view.

--TODO: Show yellow smoothed bend option on map. and nudge line.

--TODO: Result of above, editing should work on map.

--TODO: Re-grouping of controls (see Jason Hurst input).

--TODO: Popup tool help on all panes.

--TODO: Recursive track point split, user specified smoothness (i.e. depth of recursion).

--TODO: Split track to show yellow icon warning if segments less than the maximum width.

--TODO: Use "track point" not "node".

--TODO: Direct editing by dragging on map (may be easier than in elm-3d-scene).

--TODO: Really slick motion fly-through with map ??

--TODO: Optimise use of screen space, knowing viewport.

--TODO: SVG overlay - like an HUD? (elm example available)

--TODO: Scale of some kind on graphics. (Probably on map already.)

--TODO: Feat? Clicking & Dragging on Plan & Elevation views.
--Ian MacKenzie (himself) says Camera3d.ray will be useful here!

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!
--HOLD: Feat. Say if filename is too long for Magic Roads. (50 chars.)
--DROP: Scroll wheel zoom? (Quick check in Elm shows this is really nasty JS + ports solution).

