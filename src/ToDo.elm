module ToDo exposing (..)

--TODO: Deployment script !!!!!

--TODO: Display currently selected range.

--TODO: Dual-view for large monitors.

--TODO: John Bytheway suggestion.

--BUG?: Why nodes on Dennis Lane?
--Looks like Komoot not totally consistent with track points.
--Not sure what to do about that. (Might have to leave it to the user.)
-- TRY removing nodes with 0 cost metric.

--DONE: Walk the route to make new TP list.
--!! Got that image drag moving current node again.
--Hit test is way off also, possible clue.
--Might be 'cause timer's off? No.
--Oh! Seems to be side effect of having JS console open on right!

--DONE: Undo with Graph.
--THEN: Check click and double click OK
--THEN: Check profile and map views.
--THEN: Offset working again
--THEN: Use Graph to check whether edits allowed.
--THEN: Work through all the editing functions to work on the graph
--THEN: Route DSL.

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