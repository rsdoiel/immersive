cl-user-shell
==============

A brief exploration of using Lisp repl as a user shell on a Raspberry Pi. Right
now that experiment is based on [ecl](http://ecls.sourceforge.net/). Since _ecl_
is currently without maintainer (since 10/2013). An altertive Lisp on Pi would be
[ccl](http://ccl.clozure.com/).  As a result I'm putting cl-user-shell code and
customization in it's own lisp file loaded via the specific Lisp's init file (e.g.
$HOME/.eclrc for _ecl_ lisp).


To install add 

```lisp
	(load "user-shell.lisp")
```

to your .eclrc file (e.g. $HOME/$USERNAME/.eclrc). If you've placed ecl-user-shell
outside your home directory you may need to adjust the path to load user-shell.lisp.

