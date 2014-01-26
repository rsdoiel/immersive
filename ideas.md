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

+ ECL's repl with a few additions to wrap Unix commands provides the start of our Lisp experience.
+ A boot to gecko like approach should be taken for the GUI layer with the addition of a Lisp base layer

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

    An interesting alternative would be to use Linux or one of the BSD OSes as a kernel,
    providing basic hardware support, and then for the init process to start up a Lisp
    program, which would ultimately mean that there wouldn't forcibly be any detectable
    Unix -like environment underneath.

    That would provide the benefits of having the Linux/BSD folk develop hardware drivers,
    and yet leave you with a "completely Lisp" environment from the perspective of anything
    you'd see after the boot messages.


## Random ideas from reading some Lisp Machine docs

+ (defun login (username &optional (password nil)) (...)); Log into an account from outer most lisp shell

## Random shell stuff

+ the shell should have auto-complete and syntax highlighting
+ the GUI should be a boot to gecko like engine where the web is part of the native environment
+ immersive should be a little lisp sandbox of the web
+ opening a "file" should include the ability to other web assets
+ file types should support "opening" things easily based on mime-type (E.g. "open URL" or "open -t URL")
+ A sensible obvious directory layout
  - obvious could be a result of common practive (e.g. Unix layouts /etc, /home, /bin)
  - obvious could be well named (e.g. Mac's /Library, /System, /Documents, /Music, /Movies)
  - obvious should make sense in a Lisp repl
+ since this is a exploration is literate approaches useful? If so do I *knit* together something with markdown and Lip?

## Thoughts about environment architecture

1. Be interesting to have build a literate system with all source code available for reading/modification
2. git can be the glue for version control of the environment
3. the "graphics system and display" should be a web browser 
4. three levels of code
    - OS level, C based (maybe build in Linux from Scratch? or a minimal BSD?)
    - immersive base/CUI ecl plus immersive code (accessed via SSH)
    - immersive GUI is Lisp based web service(s)
    - persistance distributed disc (e.g. Dropbox, Skydrive, Amazon, Ori, arkOS-like)

Advantates? I didn't need to invent a whole OS to determine is immersive is actually
useful (I only write enough immersive to figure that out).  Why literate? Because it
may make the system more understandable to myself and anyone who takes a fancy to it.
Why web for graphics, like leveraging existing C based code using the web for display
gives me a rich UI experience out of the box. It is easy to generate HTML and probably
CSS from lisp (they are declarative). It maybe possible to create a CL compiler
that renders to asm.js either via ECL->Exscriptem->asm.js or direct transformation of
the Lisp AST.  Need to looking how ClojureScript does this. Clojure is nice but I like
CL better for my purposes (e.g. exploring some old-school going software thinking
and see how far you can take it given what we know now).

## Knit

mweave in JavaScript was my first unsophisticated poorman's literate tool based on
Markdown as the "text describing code" vehicle.  It was limitted by requiring the
text to be written in order. It was also limitted by my desire to play with a self
hosting system (e.g. you extract blocks of text from README.md to generate a 
bootstrap mweave tool that let you create a real mweave library). Given the inclinations
of a Lisp environment I think I will write Knit in Lisp, compile to standalone binary
via ECL's compiler and have it processing files from there. 

Secondary idea of Knit, could you take an existing commented Lisp program an create
a starting document for further literate development? If you could then I could bring
in interesting external Lisp programs and code bases and merge them into a literate
tree flagging them for eventual commentary (e.g. Lion's Book commentary on System 6 Unix).

The goal is two fold 

1. Keep the system understandable by me when I have serious breaks in my development
2. Hopefully have a system that is easy to grok by others who might be currious



