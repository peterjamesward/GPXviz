module ToDo exposing (..)

--TODO: 1st person view aims around the bend far too early on long segments.

--TODO: Feat. Improve terrain in presence of tight hairpins. Maybe more roadside points?

--TODO: Feat? Convert single node to arc, user selects radius and #segments.
--(I keep flipping my mind on this.)

--TODO: Bug. Yellow smoothed bend line disappears when changing view modes.

--TODO: Feat. Some way to join the start and end of a loop (although MR does this). (Tim Sinclair)
-- This can be a special action on the Overview page "Make a loop".
-- Just insert a joining TP between end and start.

--TODO: Debt. Use Node/Road, not index, in model.

--TODO: Debt. Improve removal of zero lengths so we don't have to repeat ourselves!

--TODO: Debt. Tidy up view logic.

--TODO: Debt. More modularisation.

--TODO: Debt. Single Entity list, built for current view

--TODO: Debt. Use elm-units and elm-geometry throughout, with proper units!
--Just change the Model and use the compiler. Deep breath.

