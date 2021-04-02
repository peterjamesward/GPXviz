module ToDo exposing (..)

--TODO: Display currently selected range.
--TODO: Dual (or triple) view for large monitors. Each view needs own camera.
--TODO: Use About tab to display info for each tool selected.

--DONE? Nodes and Edges. (John Bytheway.)
--THEN: Route DSL (LATER!)

--TODO: Komoot. (Would not auth last time.)
-- Thinking of adding a new view Tab visible on load (and after) for
-- route loading. This would give plenty of space for local, Strava and Komoot loads.
--TODO: Debt. More modularisation.
-- Possible way to do this is to wrap each tab's messages and move functions into
-- a source module for each tab. Graph is the new model for this.
--TODO: Debt. Reduce duplication.
--TODO: Imperial measurements in views.
--TODO: "Split track" to show warning (+ Feather icon) if segments less than the maximum segment?
--TODO: Still worth checking if we can trap any navigate-away actions.
--TODO: Smooth sideways scroll on profile. (Not worth the effort unless they complain.)

--TODO: V2 - better data types, more efficient, less crap.

%% this stops auto format