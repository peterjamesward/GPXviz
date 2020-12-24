module ToDo exposing (..)

--TODO: Change dragging effect on images.
--      Simple drag to move, ctrl-drag to rotate/tilt.

--TODO: Re-grouping of controls (see Jason Hurst input). (Problems remain on view.)
-- 1. Large image space on left;
-- 2. Upper right, tools accordion;
-- 3. Lower right, information (problems and data) accordion.

--TODO: Recursive track point split, user specified smoothness (i.e. depth of recursion).
-- this is the single node version of the bend smoother.
-- fairly simple arc fit; see notebook.

--TODO: Map terrain option (switch map style).

--TODO: Popup tool help on all accordion tabs?

--TODO: Split track to show yellow icon warning if segments less than the maximum segment?

--TODO: Remove excessive track points (like the inverse of split track but cleverer). (Jarle Steffenson).

--TODO: Use "track point" not "node".

--TODO: Optimise use of screen space, knowing viewport.

--TODO: SVG overlay - like an HUD? (elm example available) (Easier now we have camera in the Model.)
--TODO: Scale of some kind on graphics. (Probably on map already.)

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Feat. Say if filename is too long for Magic Roads. (50 chars.)

