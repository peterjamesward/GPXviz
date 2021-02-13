module TipJar exposing (tipJar)

import Element exposing (..)


tipJar =
    row [ padding 10, spaceEvenly, centerX, centerY ]
        [ image [ width <| px 200 ]
            { src = "images/tipjar.jpeg"
            , description = "Tip jar QR"
            }
        , text "Any tips go into the pot for our local hospice."
        ]
