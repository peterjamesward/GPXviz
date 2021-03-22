module ToDo exposing (..)

--TODO: Deployment script !!!!!

--TODO: Display currently selected range.

--TODO: Dual (or triple) view for large monitors. Each view needs own camera.

--WIP: John Bytheway suggestion.

--THEN: Use Graph to check whether edits allowed.
Need inverse index TP -> Edge for this.
Which means changing the return from walkTheRoute.
Need to separate the creation of the route from its access.

--THEN: Work through all the editing functions to work on the graph ("Frozen" mode).
Delete
Smooth Bend
Smooth Gradient
Nudge
Centroid Filter (might allow this over the whole route; mapped over edges)
Bezier
Insert points
Close loop
Straighten
Reverse

--THEN: Route DSL.
--THEN: "Thaw" for final tweaking of individual points.

--TODO: Komoot. (Would not auth last time.)
-- Thinking of adding a new view Tab visible on load (and after) for
-- route loading. This would give plenty of space for local, Strava and Komoot loads.

--TODO: Guidance text on tabs.

--TODO: Debt. More modularisation.
-- Possible way to do this is to wrap each tab's messages and move functions into
-- a source module for each tab. Graph is the new model for this.

--TODO: Debt. Reduce duplication.

--TODO: Imperial measurements in views.

--TODO: "Split track" to show warning (+ Feather icon) if segments less than the maximum segment?

--TODO: Still worth checking if we can trap any navigate-away actions.

--TODO: Smooth sideways scroll on profile. (Not worth the effort unless they complain.)


%% this stops auto format