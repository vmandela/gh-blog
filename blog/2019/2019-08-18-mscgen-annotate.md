---
title: Annotating Mscgen sequence diagrams
tags: mscgen
---

[Mscgen](http://www.mcternan.me.uk/mscgen/) is a nifty tool for generating
[sequence diagrams](https://en.wikipedia.org/wiki/Sequence_diagram).
One feature I miss in Mscgen is the ability to add notes within the
sequence diagram. I use some of the existing features of mscgen to add notes.
Sample output is below.

![Annotated mscgen output](/images/annotate.svg)

The tricks/hacks used are

1. Use an invisible entity for notes. The linecolor is set to white and label
   set to empty to prevent the entity from showing up in the output.

    ~~~
    A15 [label="A15"], m4 [label="M4"], notes [linecolor="white",label=""];
    ~~~

2. Create a box within the notes entity with the desired annotation. Set the
   line color to white to hide the box boundary.

    ~~~
    notes box notes [label="M4 executes in parallel with A15 loading Linux",
    linecolor="white"];
    ~~~

3. Place the annotation box at the same level as the desired operation.

    ~~~
    A15 -> A15 [label="Start loading\nLinux"],
    m4 box m4 [label="Start Video Capture\nand\nDisplay"],
    notes box notes [label="M4 executes in parallel with A15 loading Linux",
    linecolor="white"];
    ~~~

The link to the full source is [here](/images/annotate.msc)

## Live updates with mscgen

It is easy to do live updates with mscgen in discussions due to

- the tool being text based
- fast execution

I use [`iwatch`](http://iwatch.sourceforge.net/index.html) to monitor the
filesystem for any changes and running mscgen on any modified files.
I use `eog` displaying the output as it reflects any image file changes
immediately, offering a live view.

~~~{.bash}
$ cat ~/handle-iwatch.sh
#!/usr/bin/env bash
if [[ "$1" =~ .msc$ ]]; then
    mscgen -Tsvg "$1"
elif [[ "$1" =~ .dot$ ]]; then
    dot -Tpng "$1" > "${1/.dot/.png}"
fi
$ iwatch -c "/home/vmandela/handle-iwatch.sh %f" -e modify /home/vmandela/work/img
~~~
