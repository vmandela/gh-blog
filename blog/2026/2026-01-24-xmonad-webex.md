---
title: Shifting from XMonad to i3 due to Webex
tags: linux, xmonad
---

XMonad has been my window manager since 2011.
Once I got used to the workspaces and automatic assignment of windows to workspaces, I was happy.
I learnt a smattering on Haskell in 2013 which also helped in understanding the incantations in xmonad.hs.

Since Covid, my workplace moved to Webex for chat and Webex does not like Xmonad.
Everytime you try to reply to a thread, you get black overlay covering the text box.
The usual XMonad tricks with finding window name and window class with xprop do not work.
Both the text box and the overlay have the same properties.

I switched to KDE. KDE with Plasma 6 is amazing but my brain was hardwired to XMonad workspace management.
I kept trying to make KDE into XMonad.
I also tried using XMonad as the window manager for KDE but ran into the same Webex issue.

I finally tried i3 and was able to customize it into the "XMonad". 
i3 has the tiling, multi-monitor support, workspace management and window assignment to workspaces.
**Webex threaded replies work with i3.**

I only had to customize the workspace switch feature to behave the same as XMonad.
In XMonad, you can bring any workspace to the monitor you are on.
In I3, the workspaces are tied to any monitor.
I wrote a quick wrapper to do workspace reassignment to current monitor and switching to the workspace and I am all set for now.

~~~{.bash}
#!/usr/bin/env bash
WS_NUM="$1"
ACTIVE_OUTPUT=$(i3-msg -t get_workspaces \
    | jq '.[] | select(.focused==true).output')
i3-msg workspace number "$WS_NUM"
i3-msg move workspace to output "$ACTIVE_OUTPUT"
i3-msg focus output "$ACTIVE_OUTPUT"
~~~

