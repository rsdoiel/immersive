Immersive
=========

# Overview

Immersive is an exploration of a unix-like userspace implemented
in lisp.


## Dependencies

+ [SBCL](http://www.sbcl.org) version 1.2.x
+ [Quicklisp](http://quicklisp.org)
+ [ASDF 3](http://www.common-lisp.net/project/asdf/)
+ [magic-ed](https://github.com/sanel/magic-ed) - for embeded editing inside the repl
+ [rlwrap](https://tracker.debian.org/pkg/rlwrap) - for getting a convient readline experience in the repl
+ [immersive](https://github.com/rsdoiel/immersive)


## Inspirations

The idea of Lisp Machines does play a role in this exploration it is not the goal of the project to recreate their environment. At a base level there is no intention of replacing the host OS at this time (e.g. Raspbian, Debian). Instead is an exploration of what it would mean to have the host OS more closely visible in familiar function names operating at the file level.  E.g. _(ls)_ would list the files in the current directory.  While exposing the host OS through familiar function names the contents of the Lisp implementations of these functions is to return Lisp friendly objects.  Again using the _ls_ command as an example the entries should be returned as path strings that could then be consumed by other immersive functions that have knowledge of their output.


## Getting started

Currently Immersive requires the availability of ASDF 3, and Quicklisp to load
additional Lisp modules that are used in this project. The project itself 
is loaded by loading immersive.lisp from the git repository you've cloned.

1. Install [SBCL](http://wwww.sbcl.org)
2. Install [quicklisp](http://quicklisp.org)
3. Install [magic-ed](http://github.com/senal/magic-ed) as a quicklisp local package
4. Clone immersive and make a new SBCL core image.

```shell
    git clone https://github.com/rsdoiel/immersive.git
    cd immersive
    sbcl --script make-immersive-shell.lisp
```

5. Run SBCL with the immersive core image and play around

```
    sbcl --core immersive-shell
```

