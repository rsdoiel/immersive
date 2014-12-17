This is a running file or random thoughts of what Immersive Lisp might be...

# Immersive Lisp & Raspbery Pi

This project is about creating an immersive Lisp experience on a Raspberry Pi.
It is not about recreating a Symbolics or LMI era Lisp Machine. The general
hypothisis is an immersive experience could be created without ignoring
the utility of hosting operating system.  Step one is to create a pleasent
Lisp shell with Lisp functions that wrap common/helpful Unix commands
(e.g. mkdir, rmdir, ls, mv, cp, etc). Step two is to build a minimal
Linux environment with minimal services to host a user space Lisp environment.
Finally when we an immersive environment is stable enough for development
to systematically replace those non-Lisp parts that are interesting to
implement natively in Lisp.

## Lisp choice

There are a number of excellent Open Source Lisp implementations that run
on the Raspberry Pi. Currently [SBCL](http://www.sbcl.org) is the implementation
choice for Immersive. In princple it could be extended to support others
(e.g. CLISP, CMUCL, [ECLS](http://ecls.sourceforge.net), [CCL](http://ccl.clozure.com/)). 

## Unix systems and Lisp

When you read about Lisp the Lisp Machines come up promenently. Makes
sense since the language was strong enough to inspure a specific 
processors to design to leverage it. That was impressive but not necessary 
to create an immersive Lisp experience. The goal of Immersive goal is to
create a compelling Common Lisp experience on a Raspberry Pi as quickly and easily
as possible. To do that we can leverage existing Unix infrastructure and
reduce the code necessary to write.  Eventually we can replace those parts
of the system which are interesting to implement in Lisp. Those which are not
can be left for another day.


### Existing Lispy parts

+ [SBCL](http://www.sbcl.org)
+ [Quicklisp](http://quicklisp.org) and [ASDF 3](http://www.common-lisp.net/project/asdf/)
+ [magic-ed](https://github.com/sanel/magic-ed)

### Initial Unix system commands to implement

+ Basic Unix commands
  - cd
  - pwd
  - mkdir
  - rmdir
  - cp
  - mv
  - rm
  - ls
  - git
+ Leverage Existing Services via Unix init system
  - curl (http client function)
  - ws (simple static content webserver)
  - OpenSSH
  - NFS
  - Samba
  - Nginx
	
#### Problems

Some areas of dissonence are striking between the Lisp way and Unix. In Lisp 
symbols by convert to uppercase strings and Unix pathnames tend toward 
lowercase.  A successful program in Unix exits with an error code of zero 
(i.e. no error) where is a successful execution a functions feels right 
returning non-NIL values. A Unix pipe seems like it should be something
more like mapcar where the outer element sends the output of one list
element to the next and it should error out to the debug stream when there
are problems rather than just log to stderr.


## Misc

I ran across this and was surprise to see someone articulate a hunch I had...

From [lisposes](http://linuxfinances.info/info/lisposes.html) -

    An interesting alternative would be to use Linux or one of the BSD OSes as a kernel,
    providing basic hardware support, and then for the init process to start up a Lisp
    program, which would ultimately mean that there wouldnot forcibly be any detectable
    Unix -like environment underneath.

    That would provide the benefits of having the Linux/BSD folk develop hardware drivers,
    and yet leave you with a "completely Lisp" environment from the perspective of anything
    you would see after the boot messages.


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
  - obvious could be well named (e.g. an a Mac - /Library, /System, /Documents, /Music, /Movies)
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

Advantates? I did not need to invent a whole OS to determine is immersive is actually
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

mweave in JavaScript was my first unsophisticated poorman literate programming tool based on
Markdown as the "text describing code" vehicle.  It was limitted by requiring the
text to be written in order. It was also limitted by my desire to play with a self
hosting system (e.g. you extract blocks of text from README.md to generate a 
bootstrap mweave tool that let you create a real mweave library). Given the inclinations
of a Lisp environment I think I will write Knit in Lisp, compile to standalone binary (maybe
via BuildApp).

Secondary idea of Knit, could you take an existing commented Lisp program an create
a starting document for further literate development? If you could then I could bring
in interesting external Lisp programs and code bases and merge them into a literate
tree flagging them for eventual commentary (e.g. The Lion Book commentary on System 6 Unix).

The goal is two fold 

1. Keep the system understandable by me when I have serious breaks in my development
2. Hopefully have a system that is easy to grok by others who might be currious



