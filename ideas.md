
# Immersive Lisp on Raspbery Pi

This project is about creating an immersive Lisp experience on a Raspberry Pi.
It is not about recreating a Symbolics or LMI era Lisp Machine. The general
hypothisis is an emersive experience could be created without ignoring
the utility of existing Unix systems.  Step one is to create a pleasent
Lisp shell with Lisp functions that wrap helpful Unix level shell commands
(e.g. mkdir, rmdir, ls, mv, cp, etc). Step two is to build a minimal
Linux environment with minimal services to use a user space Lisp environment.
Finally when we an immersive environment is stable enough for development
to systematically replace those non-Lisp parts that are interesting to
implement in natively in Lisp.

## Lisp choice

There are a number of excellent Open Source Lisp implementations out
(e.g. CLISP, CMULisp, SBCL) but one stands out for supporting good
integration with C/C++ programs. That one is [ECL](http://ecl.sourceforge.net).
It can easily be compiled on a Rasberry Pi, can be embedded in C programs
as well as supporting Common Lisp independently. It also supports
ASFD and Quicklisp for package management.

## Unix systems to piggy back

There are a number of systems when Unix provides that can be
leverage in our Immersive Lisp environment. These include systems
support as OpenSSH, DHCP, Nginx, basic Unix filesystem support
and user management, etc. In principle these could eventually
be replace but why start with them?  Why not use them until
something better is implemented?

## System return codes

In Unix if a program successfully executes returns an error
code of 0, it is fails the errors status is 1 or greater.
In a Lisp environment it would make more sense to return
T on success of NIl on failure for a simple return value.
Another option would be return a list for the status.
To start with our system will use T for success and
NIL for failure. A global variable could contain
a list of failures with each item including information
like pid, error status code, message.



