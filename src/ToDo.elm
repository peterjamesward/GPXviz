module ToDo exposing (..)

--TODO: Deployment script !!!!!

--TODO: Tip jar ??

--TODO: Display currently selected range.

--TODO: John Bytheway suggestion.
-- New concept. "Composer" -- new View tab.
-- User can split track, at one marker (2 bits) or both (3 bits).
-- Then can compose bits together (we know where the joins are).
-- Can be ridden either way.
-- Can focus on one section, or look at whole thing (must be contiguous).
-- When outputting, can repeat any loops.
-- Should flag when sections have same start & end, encourage user to keep only one.
-- NEXT: Create Nodes & Roads from film, not from trackPoints.
-- THEN: Make all the existing functions work
-- THEN: Add new cut and splice functions

--TODO: Komoot.
-- Thinking of adding a new view Tab visible on load (and after) for
-- route loading. This would give plenty of space for local, Strava and Komoot loads.

--TODO: Guidance text on tabs.

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.
-- Loads of functions can move -- just need to explicitly list the fields they need
-- in the type signature, instead of using Model!

--TODO: Debt. Reduce duplication.

--TODO: Imperial measurements in views.

--TODO: "Split track" to show warning (+ Feather icon) if segments less than the maximum segment?

--TODO: Still worth checking if we can trap any navigate-away actions.

--TODO: Smooth sideways scroll on profile. (Not worth the effort unless they complain.)


%% this stops auto format