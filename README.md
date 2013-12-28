cl-immersive
============

A brief exploration of using [ecl](http://ecls.sourceforge.net)'s Lisp repl as a
user shell on a Raspberry Pi. The goal is to explore what an immersive Lisp environment
might look like today using comodity hardware.  While inspired by Lisp Machines
I am really not interested in recreating the past. I also think it make sense to
leverage work that has been done in non-Lisp languages (e.g. device drivers,
basic POSIX services, fast http servers like Nginx).  Finally I like that
_ecl_ is friendly with C compilers. This opens up lots of interesting oppurtunities.
_ecl_ is very close to what I need to start with and is easy to build on a Raspberry Pi.

The start of my exploration is to add the bare minumum to ecl to have a friendly
development shell (e.g. repl with readline support, easy access to Bash and vi
to bootstrap things). After that we'll see.


### ecl long term?

On 10/7/2013 Juan Jose Garcia-Rippoll [announce](http://article.gmane.org/gmane.lisp.ecl.general/10264)
he'd like to pass the baton.  Since _ecl_ as is works fine for my purposes it
is not yet an issue. In the long run I'll be watching what happens in the _ecl_
community. 

cl-immersive is targetted specifically at the Raspberry Pi but I also want to
keep options open for other CL. So to bootstrap development everything right
now is in one small Lisp file named cl-immersive.lisp.  You can add it to your
Lisp init (e.g. $HOME/.eclrc for _ecl_ lisp).


```lisp
    (load "cl-immersive.lisp")
```


