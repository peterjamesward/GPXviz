module ToDo exposing (..)

--TODO: Komoot for similar integration (for me). Then RideWithGPS maybe.
NB Komoot uses the client_id:client_secret format.
Waiting for a response from Komoot. Need client_id.

--TODO: (Samir) option to chamfer all (20-30 degree) bends and gradient changes. (Subject to adequate spacing.)

NB This will be "AutoMagic". I will take the area cost metric idea but reverse so that
TP with highest metric are split. Same approach to multiple manual passes. No need to avoid neighbours.
Put this, and Simplify, on a new tab as they are sort of complimentary?.

But that metric probably will not give enough attention to gradient changes (as cyclists do).
So maybe just use the simple bearing/slope changes we have already calculated.
Which suggests placing the autofix buttons in the Problems tabs and fixes down to current slider setting.
User needs to be told that certain errors are not fixable like this.

!!! Don't apply this if there's a tight grouping -- must be enough space to apply the chamfer.
Maybe Autosmooth could be allowed to delete nodes if there's a tight group, but detecting is awkward?
Not really. A recursive function can do that easily. Hence our autofixer can chamfer single nodes
but for a run of closely-packed nodes, might just retain the first and last. That this might create
another problem is, actually, just fine. We rely on the user as the ultimate judge!

Thus: phase 1: leave the groups, just do the isolated points. (Non-neighbours or > 5m.)
Phase 2 - remove inner points from tight groups.

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



