---
title: Jujutsu DVCS Cheatsheet
tags: git, jujutsu, version control
---

# Commands

## Initialize

~~~
$ jj git init --colocate
~~~

## Garbage Collection

To garbage collect a jujutsu repo, use

~~~
$ jj util gc
~~~

or

~~~
$ jj util gc --expire now
~~~

## Remove jj from a git repo

~~~
$ rm -rf .jj
$ git for-each-ref -- 'refs/jj/keep/*'
$ git for-each-ref  --format='%(refname)' -- 'refs/jj/keep/*' | xargs -l1 git update-ref -d
~~~

## Push a new branch

~~~
$ jj bookmark set new_branch_name
$ jj git push --allow-new
~~~

## Push an update to an existing branch

If you have a tracking branch already

~~~
$ jj bookmark set old_branch_name
$ jj git push
~~~

If you dont have a tracking branch already

~~~
$ jj bookmark track old_branch_name@origin
$ jj bookmark set old_branch_name
$ jj git push
~~~

# References

- JJ Git Command Equivalence Table - <https://jj-vcs.github.io/jj/latest/git-comparison/#command-equivalence-table>
