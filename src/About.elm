module About exposing (..)


import Element exposing (..)
import Markdown
import Msg exposing (Msg)
aboutText =
    """Thank you for trying this GPX viewer. It is freely provided without warranty.

> _This text updated 2020-12-09_

> **New tool alert** Under the Plan View is a sub-panel "On the straight". Details way down below.

**Overview** shows a route overview and summary statistics. The view is fixed on the centre of the area and there is no pan capability. You can change the presentation style using the options on the right.

From here you can also make the route into a **Loop**, if the end is near the start. If there's space, this will insert a new GPX trackpoint "behind" the start, then join the end point to this new one. You can then use Gradient and Bend smoothing near the end to tidy up. If the start and end are really close (less than a metre), it just moves the last trackpoint to be the same as the start.

You can also add **Terrain**, to get a feel for how the road looks (particularly useful in Flythroughs). _Be aware_, on a route with many trackpoints, this will take minutes to complete and will slow down everything you do. Magic Roads, it is not.

**First person** positions the viewpoint above a track _road segment_, sighted along the track. The zoom control will zoom in on the road in front. The bottom slider and arrows move between track segments. Information about the current segment is shown. **Fly-through** controls on the right hand pane: Reset, Play/Pause, Speed. Works on Third person view also.

**Third person** focuses on track _points_ and lets you fly around the current point.  An orange cone  indicates your position on the track. The slider and arrows move to other track points. Information about the current track point is shown. On the right hand side, you can click "Gradient smoother" to smooth dodgy gradients (see below).

**Elevation** shows the route, or section of it, in profile. Your focal point with the orange cone remains centred in this view. The gradient smoother is also available in this view, and it's likely to be the preferred view for gradient fixing. (For scale, the black circles are 0.2m (8 inches) diameter.)

**Gradient smoother** shows (at the bottom right) a list of track points where the gradient changes by more than the threshold you set with the slider. Click on any item in this list to move directly to that place in the view.

> To smooth a single sudden gradient transition, make sure there is no Marker (purple cone), then click the "Smooth this transition ..." button. This will replace the one transition with two smaller ones.

> To smooth a longer section of road, "Drop marker" to place a purple cone on the track; then move your current position (backwards or forwards) so that you 'bracket' a section of road. The big button will then let you 'smooth' this part of the road. You choose how bumpy you would like it: 0 is totally smooth, 1 is unchanged. You can experiment, and easily "Undo".

**Plan** is like seeing the route drawn on a flat sheet. It's ideal for focusing on bend problems. That's why the _Bend Smoother_ is there (though it's also in the Third Person view). You can see the gradients in this view by selecting the "Centre line" on the Overview panel.

> To smooth a bend (at least two road segments), position the Marker and Current node and, if possible, you will see a yellow line that is the suggested smoothed path. _Not all of these suggestions will be sensible._ You can vary the number of segments that are used (I don't know what Magic Roads would find ideal). Usual buttons to enact and Undo. It will also smooth the gradient.

> There are further tools under **On the straight**.

>  1.  When there is no dropped marker, you can **nudge** the current node left or right of the centreline by about ten metres, using the _Nudge value_ slider. It will show a preview of the new road in yellow, and this will take effect when you click **Apply nudge**. A possible usage is when you want to take a wider approach to a bend.

>  2.  Also when there is no dropped marker, you can divide the road segment immediately in front into two smaller road segments with **Divide this segment into two**. It may appear nothing has happened, because the new node is in the middle of the road and correctly interpolates between start and end.

>  3. When there _is_ a dropped marker, and you have more than one section of track bracketed, the **Straighten between markers** button appears. I wonder what is does? Yes, it makes the road completely straight. Be warned, it preserves all the nodes and their elevations, so all the gradients will be amplified, and you'll likely want to use the Gradient Smoother afterwards.

**About** -- that's this message.

Click the blue button at the page top to choose a file.

> _Peter Ward, 2020_
"""


viewAboutText : Element Msg
viewAboutText =
    row [ centerX ]
        [ paragraph [ width <| px 800 ] <| [ html <| Markdown.toHtml [] aboutText ]
        ]
