module ToDo exposing (..)

--TODO: Komoot for similar integration (for me). Then RideWithGPS maybe.
NB Komoot uses the client_id:client_secret format.
Waiting for a response from Komoot. Need client_id.

--TODO: Debt. Drop the big lists and just use arrays. Reduce duplication.

--TODO: Debt. Factor out duplicated code chunks.

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--DROP: Token refresh. (How "short-lived" are they?) Possibly only minutes!
Strava tokens last six hours, so this is not pressing.

--TODO: Popup tool help on all accordion tabs?

--TODO: "Split track" to show warning (+ Feather icon) if segments less than the maximum segment?

--NOPE: SVG overlay - like an HUD? (elm example available) (Easier now we have camera in the Model.)

--NOPE: Scale of some kind on graphics.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Say if filename is too long for Magic Roads. (50 chars.)

--IDEA: Maybe faster hit test by looking only at visible nodes. (Projection logic.)
-- That's one pass to select visibles, then only apply distance function to subset.
-- Thing is, the determination of what it visible is itself rather complex.

--DROP: Optimise use of screen space, knowing viewport.



