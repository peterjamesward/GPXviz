module ToDo exposing (..)

--TODO; Stop using Mercator. Use locally flat (cos).
--- map uses TP so unaffected.
--- Do bend smoothing in LocalCoords and convert back to GPXCoords.
--- Rationalise divergent band smoother case.

--TODO: Debt. More modularisation. Chop up the types and model. Main is toooo big.
-- Loads of functions can move -- just need to explicitly list the fields they need
-- in the type signature, instead of using Model!

--TODO: Debt. Reduce duplication.

--TODO: Autofix to work on selection also. (?)

--TODO: Put Autofix and Simplify on new Tab?

--TODO: Sponsorship link.

--TODO: Imperial measurements in views.

--TODO: Popup tool help on all accordion tabs?

--TODO: "Split track" to show warning (+ Feather icon) if segments less than the maximum segment?

--TODO: Komoot for similar integration (for me). Then RideWithGPS maybe.
--NB Komoot uses the client_id:client_secret format.
--Waiting for a response from Komoot. Need client_id.

--TODO: Still worth checking if we can trap any navigate-away actions.