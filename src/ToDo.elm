module ToDo exposing (..)

--TODO: Deployment script !!!!!

--TODO: Display currently selected range.

--TODO: Dual (or triple) view for large monitors. Each view needs own camera.

--WIP: John Bytheway suggestion.

--TODO: Detect spurious nodes by looking for degenerate triangles of edges.
These are where there is an edge with no waypoints and a colinear edge with one waypoint.
Simpler but effectively the same is to look for edges with only one waypoint, where
the waypoint is (nearly) colinear with the ends. Delete such waypoints and re-analyse.
In anomalous cases, without the way point, the adjacent points may not be nodal.

--THEN: Use Graph to check whether edits allowed.
--THEN: Work through all the editing functions to work on the graph ("Frozen" mode).
--THEN: Route DSL.
--THEN: "Thaw" for final tweaking of individual points.

--PARK: Avoid false nodes by checking the direction of neighbours, not just the count.
--This is a fairly big change, albeit limited to within Graph. Too big for now.
--Changes the definition of interestingTrackPoint to one that has neighbours in more than two directions.
--Changes the definition of edge to one that goes from one node to another, leaving in a direction.

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