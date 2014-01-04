This is a running file or random thoughts of what Immersive Lisp might be...

# Immersive Lisp & Raspbery Pi

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
integration with C/C++ programs - [ECLS](http://ecls.sourceforge.net). 
This is a real plus because we can leverage existing Unix C infrastructure 
and integrate it into our immersive Lisp environment. Finally ECLS compiles
on a Raspberry Pi and supports Lisp standards such as ASDF and Quicklisp.

## Unix systems and Lisp

When you read about Lisp the Lisp Machines come up promenently. Makes
sense since the language was strong enough to inspure a specific 
processors to design to leverage it. That was impressive but not necessary 
to create an immersive Lisp experience. Immersive Lisp's goal is to
create a compelling Lisp experience on a Raspberry Pi as quickly and easily
as possible. To do that we can leverage existing Unix infrastructure and
reduce the code necessary to write.  Eventually we can replace those parts
of the system which are interesting to implement in Lisp. Those which are not
can be left for another day.


### Existing Lispy parts

+ ECL's repl with a few additions to wrap Unix commands provides the start of our
Lisp experience.
+ Stumpwm is a X window manager rewritten in Lisp and configured in Lisp it will
be our first GUI app

### Unix systems we'll wrap

+ Basic Unix commands
	- cd
	- pwd
	- mkdir
	- rmdir
	- cp
	- mv
	- rm
	- ls
	- vi
	- git
	- curl
+ Unix C based tool chain via ECLS' approach
+ Leverage Existing Services via Unix init system
	- OpenSSH
	- NFS
	- Samba
	- Nginx
	
#### Problems

Two areas of dissonence are striking between the Lisp way and Unix. In Lisp 
symbols by convert to uppercase strings and Unix pathnames tend toward 
lowercase.  A successful program in Unix exits with an error code of zero 
(i.e. no error) where is a successful execution a functions feels right 
returning non-NIL values.

## Existing Lisp infrastructure

Quicklisp/ASDF provide many Lisp packages some can be leverage to minimize
the ammount of code needed to get Immersive off the ground. Here's a list
of components that might aid that effort.

+ [cl-cron](http://quickdocs.org/cl-cron) - a crontab tool
+ [cl-utilities](http://common-lisp.net/project/cl-utilities/) - common lisp semi-standard utilities
+ [monkeylib-utilities](http://quickdocs.org/monkeylib-utilities/) - Peter Seibel's utilities (e.g. JSON parser)
+ [cl-fad](http://quickdocs.org/cl-fad/) - portable pathname library
+ [osicat](http://www.common-lisp.net/project/osicat/) - A lispy [OS API](http://www.common-lisp.net/project/osicat/manual/osicat.html), may be useful in moving from wrapping Unix command to replacing them.
+ [external-program](https://github.com/sellout/external-program) - provides portable ways of accessing external programs from the Lisp environment
+ [cl-syslog](http://quickdocs.org/cl-syslog/) - Common Lisp syslog interface

## Packaging things

+ [ASDF](http://common-lisp.net/~mmommer/asdf-howto.shtml) How-to toturial

## Misc

I ran across this and was surprise to see someone articulate a hunch I had...

From [lisposes](http://linuxfinances.info/info/lisposes.html) -

	An interesting alternative would be to use Linux or one of the BSD OSes as a kernel, providing basic hardware support, and then for the init process to start up a Lisp program, which would ultimately mean that there wouldn't forcibly be any detectable Unix -like environment underneath.

	That would provide the benefits of having the Linux/BSD folk develop hardware drivers, and yet leave you with a "completely Lisp" environment from the perspective of anything you'd see after the boot messages.

## Random ideas from reading some Lisp Machine docs

+ (defun login (username &optional (password nil)) (...)); Log into an account from outer most lisp shell


