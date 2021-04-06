module ToDo exposing (..)

--TODO: Display currently selected range. (This may fall out of Graph stuff.)
--TODO: Dual (or triple) view for large monitors. Each view needs own camera.
--TODO: Use About tab to display info for each tool selected.

--WIP Nodes and Edges. (John Bytheway.)

Revisit all the editing. Both the gaurds and the implementation.
Principle of least work, not new graphs.

Index-preserving edits: these update track point positions only, and we
can rely on the route and the track points matching.
These _can_ work over nodes and hence affect none or more edges.
1. Update affected edges, whether in whole or in part.
2. Update affected nodes.
3. Re-walk route to apply new canon.
4. (Reverse index not affected.)
    Nudge
    Centroid
    Smooth gradient
    Straighten
    Drag on map

Vertex-preserving edits: must rely on latlon of vertices being unchanged,
and track points of _one_ edge only changed. May _not_ inlcude nodes.
1. Update affected edge's track point list
2. Re-walk the route
3. Rebuild reverse index.
    Delete
    Insert
    Two for one
    Splines
    Smooth bend
    Strava segment

--THEN: Route DSL (LATER!) (Possibly with geocode labels.)

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

--TODO: V2 - better data types, more efficient, less crud.

%% this stops auto format