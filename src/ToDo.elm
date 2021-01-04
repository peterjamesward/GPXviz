module ToDo exposing (..)

--TODO: Make the auth code generate a specified app-level Msg when auth completes.

--TODO: "Compatible with Strava" logo.

--TODO: Strava data fetches to use token!
Add the header Authorization: Bearer <token>
NB Don't really want the Done state in Auth. We want to keep the token available.
Monday, change this. Just make the user details a regular call, not in the state machine, or otherwise keep the token.

--TODO: New view to select Strava routes. Do this before the segment thing Samir asked for!
{-
You must link back to all original Strava data sources presented in your application using the following text format, “View on Strava”.

Text link should be legible.
Text link should be identifiable as a link by using one of the following type treatments: bold weight, underline, or orange color #FC4C02.
-}

--TODO: Strava logout option.

--TODO: Option to disable dragging on map view. (Donald Black)
--TODO: Map terrain option (switch map style).
Both these could go into a Map info tab.

--TODO: Komoot for similar integration (for me). Then RideWithGPS maybe.
NB Komoot uses the client_id:client_secret format.

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


--TODO: Debt. Drop the big lists and just use arrays. Reduce duplication.

--TODO: Debt. Factor out duplicated code chunks.

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.

--TODO: Token refresh. (How "short-lived" are they?) Possibly only minutes!
Strava tokens last six hours, so this is not pressing.


--TODO: Popup tool help on all accordion tabs?

--TODO: Split track to show warning (+ Feather icon) if segments less than the maximum segment?

--TODO: SVG overlay - like an HUD? (elm example available) (Easier now we have camera in the Model.)

--TODO: Scale of some kind on graphics.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Say if filename is too long for Magic Roads. (50 chars.)

--TODO: Maybe faster hit test by looking only at visible nodes. (Projection logic.)
-- That's one pass to select visibles, then only apply distance function to subset.
-- Thing is, the determination of what it visible is itself rather complex.

--DROP: Optimise use of screen space, knowing viewport.



