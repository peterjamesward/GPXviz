module ToDo exposing (..)

--TODO: Deployment script !!!!!

--TODO: Display currently selected range.

--TODO: Highlight doubled-up trackpoints (Strava does this a lot).
-- This may fold into the Bytheway work.

--TODO: Dual-view for large monitors.

--TODO: John Bytheway suggestion.
-- now using automatic analysis to convert to edges in a graph.
--NOTE: User must make sure that any crossroads are horizontal, not just same elevation.
--DONE: Derive edge list from route.
--THEN: Convert route to new one derived from edge traversals.
--THEN: Edges used backwards need to have centre-line offset applied (global value).
--THEN: New view that shows nodes and edges.
--THEN: Allow editing of Edges (one at a time?)
--THEN: Compose edges to produce new output. With looping.

--TODO: Komoot.
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