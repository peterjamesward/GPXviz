module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Markdown
import Msg exposing (Msg)


aboutText =
    """Thank you for trying this GPX viewer. It is freely provided without warranty.

> **It's all changed**

> _This text updated 2020-12-11_



**About** -- that's this message.

Click the blue button at the page top to choose a file.

> _Peter Ward, 2020_
"""


viewAboutText : Element Msg
viewAboutText =
    row
        [ centerX
        , Background.color <| rgb255 220 220 200
        , padding 20
        , Border.width 2
        , Border.color <| rgb255 50 50 50
        , clipY
        , scrollbarY
        ]
        [ paragraph
            [ width <| px 800
            , height <| maximum 600 fill
            ]
          <|
            [ html <| Markdown.toHtml [] aboutText ]
        ]
