module Loop exposing (..)

type
    Loopiness
    -- Not sure this is the best place, but better here than Main.
    = NotALoop Float
    | IsALoop
    | AlmostLoop Float -- if, say, less than 200m back to start.

