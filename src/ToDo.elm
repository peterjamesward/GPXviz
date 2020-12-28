module ToDo exposing (..)

--TODO: Double clicking is crashing (only?) after track pruning.

--TODO: Tool to reduce excessive track points. (Jarle Steffenson).
-- I like the idea of applying a cost function to each node, using a fold,
-- collating local minima, such that we identify track points that would have
-- minimum impact on track if removed. Then one click to remove, easy undo.
-- Button says "Remove xxx track points". Really simple to use, no options.

--TODO: Map terrain option (switch map style).

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Popup tool help on all accordion tabs?

--TODO: Optimise use of screen space, knowing viewport.

--TODO: Split track to show text (+ Feather icon) warning if segments less than the maximum segment?

--TODO: SVG overlay - like an HUD? (elm example available) (Easier now we have camera in the Model.)

--TODO: Scale of some kind on graphics. (Probably on map already.)

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Say if filename is too long for Magic Roads. (50 chars.)

--TODO: Maybe faster hit test by looking only at visible nodes. (Projection logic.)
-- That's one pass to select visibles, then only apply distance function to subset.
-- Thing is, the determinatio of what it visible is itself rather complex.



