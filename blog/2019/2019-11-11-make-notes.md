---
title: Notes on Make
tags: make
---

This is a collection of notes I have accumulated while working with make. The make manual is a good place to start looking for answers.

<https://www.gnu.org/software/make/manual/make.html>

## Debugging makefiles

## gmake info command


The `info` command can be used to dump information from makefile.

<https://www.gnu.org/software/make/manual/make.html#Make-Control-Functions>

~~~
$(info CFLAGS=$(CFLAGS))
~~~

Similarly one can use the `error` and `warning` commands can be used for debugging.

### Remake

Remake <http://bashdb.sourceforge.net/remake/> is a enhanced version of GNUMake
with enhanced tracing and a builtin debugger. I have been using remake as a
replacement for make on my development machine and am yet to run into any
issues.

~~~
alias make=remake
~~~

Now you can use 

~~~
make --trace
~~~

to get better information on why a target is being built/rebuilt.

The `-B` option of remake triggers an unconditional rebuild of a specified target.

#### Related reading

1. Debugging makefiles with remake - <https://www.usenix.org/legacy/events/lisa11/tech/full_papers/Bernstein.pdf>
2. Remake home page - <http://bashdb.sourceforge.net/remake/>
3. Remake GitHub repository - <https://github.com/rocky/remake>

## Parallelizing recursive make

To ensure the `-j N` flag is used when doing recursive make, use `$(MAKE)` when
invoking the submake. This ensures that the gmake jobserver is used correctly. Otherwise
you might see warnings such as

~~~
make[1]: warning: jobserver unavailable: using -j1.  Add '+' to parent make rule.
~~~

### Related reading

1. How the make variable works - <https://www.gnu.org/software/make/manual/make.html#MAKE-Variable>
1. GNUMake Jobserver Implementation - <http://make.mad-scientist.net/papers/jobserver-implementation/>
1. Recursive make considered harmful - <http://aegis.sourceforge.net/auug97.pdf>

## Miscellaneous

### Handling symbolic links

When one of the dependencies is a symbolic link, make checks the modification time of the file 
referenced by the link. This is sensible behaviour and probably the reason it is not called out
in the gnu make manual.

There is also the "-L" option which causes make to consider the modification time of the symlink
as well. This is option is useful when one wants to "touch symlink" and run make.

~~~
`-L'
`--check-symlink-times'

    On systems that support symbolic links, this option causes make to consider the timestamps
    on any symbolic links in addition to the timestamp on the file referenced by those links.
    When this option is provided, the most recent timestamp among the file and the symbolic
    links is taken as the modification time for this target file.
~~~
