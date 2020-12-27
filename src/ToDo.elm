module ToDo exposing (..)

--TODO: Maybe faster hit test by looking only at visible nodes. (Projection logic.)
-- That's one pass to select visibles, then only apply distance function to subset.

--TODO: Split track to show text (+ Feather icon) warning if segments less than the maximum segment?

--TODO: Tool to reduce excessive track points. (Jarle Steffenson).

--TODO: Map terrain option (switch map style).

--TODO: Popup tool help on all accordion tabs?

--TODO: Optimise use of screen space, knowing viewport.

--TODO: SVG overlay - like an HUD? (elm example available) (Easier now we have camera in the Model.)
--TODO: Scale of some kind on graphics. (Probably on map already.)

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Say if filename is too long for Magic Roads. (50 chars.)

