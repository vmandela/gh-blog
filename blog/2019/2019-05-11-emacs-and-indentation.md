---
title: Emacs and Indentation
tags: emacs
---

Though I have been using emacs for few years now, I faced issues with
controlling the indentation. Consider the below setting in vim to
configure the indentation for all source files in a directory.

- Use tabs for indentation
- Treat tab width as 8 spaces

~~~
au BufEnter /code/kernel/*/*.[ch] setlocal tabstop=8 shiftwidth=8 noexpandtab
~~~

I tried [dtrt-indent](https://github.com/jscheid/dtrt-indent) which
automatically configures indentation based on existing indentation in
the file. This approach works well for existing source files but does
not configure indentation for new source files.

I settled on replicating the vim behaviour exactly by using [per
directory local
variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables).

~~~
(dir-locals-set-class-variables
 'kernel-src-directory
 '(
   (c-mode . (
	      (c-basic-offset . 8)
	      (evil-shift-width . 8)
	      (indent-tabs-mode . t)
	      )
	   )
   )
 )
(dir-locals-set-directory-class
    "/code/kernel" 'kernel-src-directory)
~~~
