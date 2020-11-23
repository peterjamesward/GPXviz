module ToDo exposing (..)

--TODO: Autofix bends with circular arcs. Start simple.
--TODO: Try chamfer for horizontal turns. (May turn out as special case of previous.)
--TODO: Some way to join the start and end of a loop (although MR does this). (Tim Sinclair)
-- (Thinking: bracket points near the start and end, and have special button to create midpoint.
-- Or, in 1st person mode, drop marker at/near start, move to/near end, have special Join button.
-- Then just use other tools for smoothing.
--TODO: Show gradients or transition in 1st person? (Jarie Steffenson)
--TODO: Improve removal of zero lengths so we don't have to repeat ourselves!
