Immersive
=========

# Overview

Immersive is an expliremental Lisp environment with a unix-like influenced. A number of unix-like utilities like _cat_, _ls_, _cd_ have been reimplemented as toplevel functions available to navigate the file system and orchestrate operations on it.


## Current exploration

Immersive is now being developed using [SBCL](http://www.sbcl.org) on the Raspberry Pi.  While it should be portable to other Raspberry Pi based Common Lisp the current gloal is to seek the shape of what immersive will become.


## Previous exploration

Initially _ecl_ was used as it was the first Common Lisp I got reliably running on the Raspberry Pi based on notes in the R-Pi community.  Eventually I switch to Closure Common Lisp (ccl) which ran faster and had the advantage that of a clear maintainer.  In the mean time _SBCL_ became available and that was adopted. Currently development is proceeding with _SBCL_ 1.2.6 (as of 2014-12-16).


## Inspirations

The idea of Lisp Machines does play a role in this exploration it is not the goal of the project to recreate their environment. At a base level there is no intention of replacing the host OS at this time (e.g. Raspbian, Debian). Instead is an exploration of what it would mean to have the host OS more closely visible in familiar function names operating at the file level.  E.g. _(ls)_ would list the files in the current directory.  While exposing the host OS through familiar function names the contents of the Lisp implementations of these functions is to return Lisp friendly objects.  Again using the _ls_ command as an example the entries should be returned as path strings that could then be consumed by other immersive functions that have knowledge of their output.


## Getting started

Currently Immersive requires the availability of ASDF 3, and Quicklisp to load
additional Lisp modules that are used in this project. The project itself 
is loaded by loading immersive.lisp from the git repository you've cloned.


```shell
    git clone https://github.com/rsdoiel/immersive.git
    cd immersive
    sbcl
```

Once SBCL is running

```
    (load "./immersive.lisp")
```

