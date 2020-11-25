module ToDo exposing (..)


--TODO: (2) On chamfers, allow more space for larger transitions, so user can recurse if needed.
-- or provide numSegs slider and try to fit a certain radius (vertical incircle, should be easy now).

--TODO: (3) Some way to join the start and end of a loop (although MR does this). (Tim Sinclair)
-- (Thinking: bracket points near the start and end, and have special button to create midpoint.
-- Or, in 1st person mode, drop marker at/near start, move to/near end, have special Join button.
-- Then just use other tools for smoothing.
-- This can be a special action on the Overview page "Make a loop".
-- Just insert a joining TP between end and start.

--TODO: (4) Optional gradient-coloured centre line. (1st and Plan views) (Jarie Steffenson)

--TODO: Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Tidy up view logic.

--TODO: Single Entity list, built for current view

--TODO: Use elm-units and elm-geometry throughout!

--BACKLOG: Cater for parallel and divergent segments.
