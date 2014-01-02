
# Immersive Lisp on Raspbery Pi

This project is about creating an immersive Lisp experience on a Raspberry Pi.
It is not about recreating a Symbolics or LMI era Lisp Machine. The general
hypothisis is an emersive experience could be created without ignoring
the utility of existing Unix systems.  Step one is to create a pleasent
Lisp shell with Lisp functions that wrap common/helpful Unix commands
(e.g. mkdir, rmdir, ls, mv, cp, etc). Step two is to build a minimal
Linux environment with minimal services to host a user space Lisp environment.
Finally when we an immersive environment is stable enough for development
to systematically replace those non-Lisp parts that are interesting to
implement natively in Lisp.

## Lisp choice

There are a number of excellent Open Source Lisp implementations out
(e.g. CLISP, CMULisp, SBCL) but one stands out for supporting good
integration with C/C++ programs. That one is [ECL](http://ecls.sourceforge.net).
It can easily be compiled on a Rasberry Pi, can be embedded in C programs
as well as supporting Common Lisp independently. It also supports
ASFD and Quicklisp for package management. This gives us allot of
practical options while morphing our Unix environment into an
immersive Lisp one.


## Piggy back on a Unix systems

There are a number of systems Unix provides that can be
leverage in our Immersive Lisp environment. These include systems
support as OpenSSH, DHCP, Nginx, basic Unix filesystem support
and user management, device drives. In principle these could 
eventually be replace but why start with them?  Why not use them 
until something better is implemented?

Two areas of dissonence are striking between the Lisp way and
Unix. In Lisp symbols by default convert to uppercase strings
and Unix pathnames tend toward lowercase.  A successful program
in Unix exits with an error code of zero (i.e. no error) where
is a successful execution feels like it should be T and
an unsuccessful one should be NIL in Lisp. Fortunately taking
with a wrapper approach in Immersive this is just a function
call or two away.




