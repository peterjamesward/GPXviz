module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Markdown
import Msg exposing (Msg)
import Utils exposing (view3dDimensions, view3dHeight, view3dWidth, viewMapHeight, viewMapWidth)


aboutText =
    """## Thank you for trying GPXmagic. It is freely provided without warranty.

> _This text updated 2021-01-05_

## Changes

- Major new tool in the Bend & Gradient problems tabs. Don't over-use this. It attempts to deal with all the problems visible in the list, either by deleting a single track point, or replacing one with two (a 'chamfer' operation). The _intended_ use is that you sort out major issues individually then use this for the "long tail" of minor issues. The risk is you just get a lot of very close points with little real value. But, hey, it's a tool; use it how you will. You can always Undo.

- You can paste in the complete route URL from Strava, and GPXmagic will retain only the route ID.

- Bend Problems tab was listing the gradient problems (woops).

- You choose whether or not to drag track points on the map. Default is not. Use the new Map Options tab to change this behaviour.

## Guidance on use

Load a local GPX file by clicking on the aptly-labelled button. Or connect to Strava by clicking on the equally-apt brand-compliant button and authorize GPXmagic to access your routes. You can then enter a numeric route ID (you'll find this at the end of the URL for a route) and get the GPX by clicking "Fetch route". The Strava connection is valid for six hours.

Once a file is loaded, **Third person**, **First person**, **Elevation**, **Plan**, and **Map** provide views on the course. On the right hand side are numerous options that I will elaborate below. You can mix and match the views and the option panels.

**Map view** You can now click on the track to select a track point. When you first do this, you will probably see an Orange marker and a smaller purple marker superimposed. These remain together until you "Drop" the marker, after which clicking will move the Orange marker only until you "Clear" the purple marker. This makes for rapid selection of a section of route -- just click once to place both pointers, drop the purple one, click somewhere else. You can then use Nudge, Straighten, Bend smoothing (anything, in fact) on that range.

**Summary** summarises the GPX information. This provides error messages if the file is not what we're expecting.

**Road data** gives information about the current road segment -- the one immediately "in front of" the orange marker.

**Visual styles** lets you choose what you want shown. The effects are immediate in all views.

**Loop maker** is handy if your start and end points are close. You can make the track into a loop. This will either just move the last track point (if they are really close), or will insert a new one. Once your track is a loop, you can move the orange pointer and choose any point as the start/finish. (You can use this as a way to apply tools to the "real" start/finish area, moving the start back when you're done.)

**Fly-through** will move the current point around the track at variable speed. This works in all views but 1st and 3rd person are most appropriate.

**Smooth gradient** groups tools that are useful for smoothing gradients. You can replace the current track point with two; often this is enough to smooth a coarse gradient change. Beyond that, you can select a longer section of road by dropping and moving the marker (appears as a purple cone). Then use the button to apply smoothing to the selected track segments, and you can choose to retain some of the original flavour by increasing the "Bumpiness factor".

**Nudge node** provides direct manipulation of the current point (orange marker). You can move it vertically and side-to-side by five metres. You can apply repeatedly if that's not enough.

**Smooth bend** works only with a selected range. It tries (not always successfully) to fit a circular arc that is tangent to the segments that are marked. Moving the current point and the marker will provide different options. Increase the number of road segments for a smoother bend. If you can't get a nice looking bend, it may be worth adding some more track points (see below) and trying again.

**Straighten** is like an opposite of bend smoothing. When you have a "nearly straight" that you want to be "really straight", this is your friend. It retains track point elevation, and just marshals them into a straight line, so you may need other tools to finish the job.

**Trackpoints** allows you to add track points before and after the current point (same as in the Gradient panel). Another option, useful on long straights near bends, is to add a new point in the middle of a road segment. Repeat as required. Delete will delete the current track point.

**Gradient problems** and **Bend problems** highlight track points that may be of interest. Click on any entry to make that current. The AutoFix option is available, but should not be taken for granted.

Click the blue button at the page top to choose a file.

**Remember to save** your changes often. The Save button writes to your download folder only (this is a security limitation of browsers).

## Legally required notice

Compatible with Strava, for the purpose of loading segment data.

> _Peter Ward, 2020_
"""


viewAboutText : Element Msg
viewAboutText =
    row
        [ centerX
        , Background.color <| rgb255 220 220 200
        , clipY
        , scrollbarY
        , height <| px viewMapHeight
        , padding 20
        ]
        [ paragraph
            [ width <| px viewMapWidth
            , height <| px viewMapHeight
            ]
          <|
            [ html <| Markdown.toHtml [] aboutText ]
        ]
