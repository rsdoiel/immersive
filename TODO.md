
+ (ls) does not pickup the change of directory information
+ (ls) needs to have suitable defaults (e.g. listing a directory normally lists the contents of the directory unless a regexp is provided)


These are some todo items to get be working in SBCL.

+ Re-organize commands and bundle in a package called immerse.
+ wrap git commands so I run them from lisp repl
+ wrap common Unix commands
	- cd
	- pwd
	- mkdir
	- rmdir
	- ls
	- cp
	- mv
	- rm
	- chown
	- chmod
	- wc
	- find
	- grep
+ Add other useful commands like mime-type-of
+ create a secure machanism to send computation to other immmersive instances (e.g. via secure http/webRTC channel)

