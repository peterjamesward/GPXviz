module ToDo exposing (..)

--TODO: "Bug" I think where bend section has steep gradient and extrapolation to tangent point leads to egregious elevation change around the bend. Should probably use A.ele and D.ele, not extrapolated.

--TODO: Bug. Yellow line disappears when changing view modes.

--TODO: Bug. Drop the concept of "nearby" nodes for centering views.

--TODO: Feat. Cater for parallel and divergent segments.
-- I started this but got all confused again, mostly about relative position of intersect point.

--TODO: Feat. Some way to join the start and end of a loop (although MR does this). (Tim Sinclair)
-- This can be a special action on the Overview page "Make a loop".
-- Just insert a joining TP between end and start.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Debt. Tidy up view logic.

--TODO: Debt. Single Entity list, built for current view

--TODO: Debt. Use elm-units and elm-geometry throughout!

