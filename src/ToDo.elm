module ToDo exposing (..)

--TODO: Deployment script !!!!!

--TODO: Display currently selected range.

--TODO: Highlight doubled-up trackpoints (Strava does this a lot).
-- This may fold into the Bytheway work.

--TODO: Dual-view for large monitors. (Triple view?)

--TODO: John Bytheway suggestion.
--NOTE: *User* must make any crossroads horizontal, not just same elevation.

--THEN: Centre-line offset (global).
-- I wonder whether the first pass of an edge is not the canonical (height difference).
-- Applying offset to roads doesn't really work - we need to apply to track points
-- and the logic we use for Nudge should work.
-- MAYBE makes sense to compute the offset direction when we canonicalise.

--THEN: Allow editing of Edges (graph function to check that TP on same edge => editable)
--THEN: New view that shows nodes and edges.
--THEN: Compose edges to produce new output. With looping (simple DSL with prompted editing).
--ALSO: Allow to switch back (thaw) to non-constrained track points (with spacing).

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