module ToDo exposing (..)

--TODO: Display currently selected range.
--TODO: Dual (or triple) view for large monitors. Each view needs own camera.

--WIP: John Bytheway suggestion.

--THEN: Use Graph to check whether edits allowed.
The check same edge logic could return the appropriate pair as Maybe (Int, Int) -- I like that; it nearly works.
Might be helpful to display graph data for current node. ++++

--THEN: Work through all the editing functions to work on the graph ("Frozen" mode).

OPERATIONS THAT APPLY UPDATES TO TRACKPOINTS
Nudge
Straighten
Smooth Gradient
Centroid Filter

OPERATIONS THAT MODIFY THE TRACKPOINT LIST
Delete
Smooth Bend <WIP>
Insert points
Split point
Bezier
Strava segment
Simplify

UNUSUAL OPERATIONS (we need not provide these in Graph mode)
Close loop      (OK, but not important as can be done befor analysis)
Reverse         (inappropriate so not at all, superseded by DSL)

OPERATIONS THAT CAN WORK ON A SINGLE NODE
Nudge vertically
Drag on map

** It seems we should only allow ranges that lay on a common edge, excluding nodes. **
-- Could also allow operations where one node is selected and other marker on an edge --
-- Offset should be 0 whilst editing. Advisory only.

Should now be fairly easy to send the new TP list to Graph, where we use the reverse index
to identify affected edge. We replace ALL TP on that edge, then walk the route again.

--THEN: "Thaw" for final tweaking of individual points.
--THEN: Route DSL (LATER!)

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