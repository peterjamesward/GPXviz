module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Markdown
import Msg exposing (Msg)


aboutText =
    """Thank you for trying this GPX viewer. It is freely provided without warranty.

> _This text updated 2020-12-15_

> **Changes**
> - Flythrough shows distance from start.

The file load and save buttons are the same, almost everything else has changed. It's a rework of the user experience. I hope this is largely for the better, though there are inevitably some compromises.

The selection button style is changed, and there is no _Overview_ panel. Under **About**, you see this text, which will scroll within the shaded box. The other options work only once a file is loaded.

Once a file is loaded, **Third person**, **First person**, **Elevation** and **Plan** provide four views on the course. On the right hand side are numerous options that I will elaborate below. You can mix and match the views and the option panels.

**File** just summarises the GPX information. This will (soon) provide error messages if the file is not what we're expecting.

**Road data** gives information about the current road segment -- the one immediately "in front of" the orange marker.

**Visual styles** lets you choose what you want shown. The effects are immediate in all views.

**Loop maker** is handy if your start and end points are close. You can make the track into a loop. This will either just move the last track point (if they are really close), or will insert a new one. Once your track is a loop, you can move the orange pointer and choose any point as the start/finish. (You can use this as a way to apply tools to the "real" start/finish area, moving the start back when you're done.)

**Fly-through** will move the current point around the track at variable speed. This works in all views but 1st and 3rd person are most appropriate.

**Smooth gradient** groups tools that are useful to, um, smooth gradients. You can just insert track points (nodes) before or after the current point. Often this is enough to smooth a coarse gradient change. Beyond that, you can select a linger section of road by dropping and moving the marker (which appears as a purple cone). Then use the button to apply smoothing to the selected track segments, and you can choose to retain some of the original flavour by increasing the "Bumpiness factor".

**Nudge node** provides direct manipulation of the current point (orange marker). You can move it vertically and side-to-side by five metres. You can apply repeatedly if that's not enough.

**Smooth bend** works only with a selected range. It tries (not always successfully) to fit a circular arc that is tangent to the segments that are marked (that's geometry, right there). Moving the current point and the marker will provide different options. Increase the number of road segments for a smoother bend. If you can't get a nice looking bend, it may be worth adding some more track points (see below) and trying again.

**Straighten** is like an opposite of bend smoothing. When you have a "nearly straight" that you want to be "really straight", this is your friend. It retains track point elevation, and just marshals them into a straight line, so you may need other tools to finish the job.

**Trackpoints** allows you to add track points before and after the current point (same as in the Gradient panel). Another option, useful on long straights near bends, is to add a new point in the middle of a road segment. Repeat as required. Delete will delete the current track point.

**Gradient problems** and **Bend problems** highlight track points that may be of interest. Click on any entry to make that current.

Click the blue button at the page top to choose a file.

**Remember to save** your changes often. The Save button writes to your download folder only (this is a security limitation of browsers).

> _Peter Ward, 2020_
"""


viewAboutText : Element Msg
viewAboutText =
    row
        [ alignLeft
        , alignTop
        , width (px 880)
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
