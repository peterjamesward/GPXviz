module ToDo exposing (..)

--TODO: Display currently selected range.
--TODO: Dual (or triple) view for large monitors. Each view needs own camera.

--WIP: Nodes and Edges. (John Bytheway.)

--THEN: Use Graph to check whether edits allowed.
--THEN: Work through all the editing functions to work on the graph ("Frozen" mode).

DONE
    OPERATIONS THAT MODIFY THE TRACKPOINT LIST AND THUS GRAPH EDGES
    Strava segment
    Bend Smoother.
    Delete.
    Split point
    Insert points
    Bezier (but beware adding Offset when track points are close)
    Simplify

    Nudge

OPERATIONS THAT APPLY UPDATES TO TRACKPOINTS BUT DO NOT CHANGE THE GRAPH
( and could traverse nodes and multiple edges, just walking the track point list )
( but we shall restrict to single edge for consistency and simplicity )
Straighten
Smooth Gradient
Centroid Filter

OPERATIONS THAT CAN WORK ON A SINGLE NODE AND SHOULD BE SUPPORTED
Nudge vertically
Drag on map

UNUSUAL OPERATIONS (we need not provide these in Graph mode)
Close loop      (OK, but not important as can be done befor analysis)
Reverse         (inappropriate so not at all, superseded by DSL)

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

--TODO: V2 - better data types, more efficient, less crap.

%% this stops auto format