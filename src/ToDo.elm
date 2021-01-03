module ToDo exposing (..)

--TODO: Option to disable dragging on map view. (Donald Black)

--TODO: (Samir) option to chamfer all (20-30 degree) bends and gradient changes. (Subject to adequate spacing.)

NB This will be Autosmooth. I will take the area cost metric idea but reverse so that
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

--TODO: Is it legal to hoover Strava/Veloviewer segment data?
I'm pretty sure it complies with their Ts&Cs. It's not their data anyway.
I think I can go straight to Strava API. Except auth is a pain, so no.
Let's put a fetch from VW URL option on the page.
Then we can check it will work without Strava login or, if you are looking at a private segment, somehow VW will work (!).

--TODO: Under the Samir tab, import GPX segment for elevation matching.
NB Veloviewer returns d3 compressed data for distance, elevation and gradient.
(Which is just the Strava Segment Streams compressed by d3, methinks.)
With the d3 routines, it should be easy enough to decompress and use these for accurate elevation data.

--TODO: Debt. Drop the big lists and just use arrays. Reduce duplication.

--TODO: Debt. Factor out duplicated code chunks.

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Map terrain option (switch map style).

--TODO: Optimise use of screen space, knowing viewport.

--TODO: Popup tool help on all accordion tabs?

--TODO: Split track to show warning (+ Feather icon) if segments less than the maximum segment?

--TODO: SVG overlay - like an HUD? (elm example available) (Easier now we have camera in the Model.)

--TODO: Scale of some kind on graphics.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Say if filename is too long for Magic Roads. (50 chars.)

--TODO: Maybe faster hit test by looking only at visible nodes. (Projection logic.)
-- That's one pass to select visibles, then only apply distance function to subset.
-- Thing is, the determinatio of what it visible is itself rather complex.



