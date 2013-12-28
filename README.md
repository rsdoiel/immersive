cl-user-shell
==============

A brief exploration of using [ecl](http://ecls.sourceforge.net)'s Lisp repl as a
user shell on a Raspberry Pi. The goal is to explore what an immersive lisp environment
might look like today using comodity hardware.  While inspired by Lisp Machines
I am really not interested in recreating the past. I also think it make sense to
leverage work that has been done in non-Lisp languages (e.g. device drivers,
basic POSIX services, fast http servers like Nginx).  Finally I like that
_ecl_ is friendly with C compilers. This opens up lots of interesting oppurtunities.
_ecl_ is very close to what I need to start with and is easy to build on a Raspberry Pi.

The start of my exploration is to add the bare minumum to ecl to have a friendly
development shell (e.g. repl with readline support, easy access to Bash and vi
to bootstrap things). After that we'll see.


### ecl long term

On 10/7/2013 Juan Jose Garcia-Rippoll [announce](http://article.gmane.org/gmane.lisp.ecl.general/10264)
he'd like to pass the baton.  Since ecl as is works fine for my purposes it
might not be an issue.  In the long run I'll be watching what happens in the _ecl_
community. 

An alternative that I looked at before deciding on _ecl_ for this project was
[Clozure Common Lisp](http://ccl.clozure.com). It was close to my needs. It runs
on a Raspberry Pi but it requires an image to bootstrap itself.  If I want to
move this project to another system whichout CCL support then I'd be stuck.  As a
result I'm putting cl-user-shell code and customization in it's own lisp file
loaded via the specific the host Lisp's init file (e.g. $HOME/.eclrc for _ecl_ lisp).


Add the following to your Lisp init to install (e.g. $HOME/$USERNAME/.eclrc).

```lisp
	(load "user-shell.lisp")
```


