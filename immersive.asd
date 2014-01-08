;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:immersive
  (:use :cl :asdf :cl-fad))

(in-package #:immersive)

(defsystem #:immersive
	   ;:name "immersive"
	   ;:version "0.0.0"
	   ;:author "R. S. Doiel"
	   ;:license "BSD 2-clause License"
	   ;:description "An immersive Lisp environment intended to run on a Raspberry Pi Model B or A"
	   :components ((:file "immersive.lisp")
			(:file "cat.lisp")
			(:file "cd.lisp")
			(:file "edit.lisp")
			(:file "exit.lisp")
			(:file "git-scm.lisp")
			(:file "immersive.lisp")
			(:file "ls.lisp")
			(:file "mkdir.lisp")
			(:file "pwd.lisp")
			(:file "rmdir.lisp")
			(:file "shell.lisp")))



