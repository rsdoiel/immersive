;;;;
;;;; ilsh - Immersive Lisp Shell. This an experiment in creating an
;;;; hybrid Lisp/Unix experience.
;;;;

;;;
;;; Built in commands (these are native Common Lisp
;;; implementations of common Unix commands)
;;;
;;(load "immersive.lisp")
;;(immersive)

;;;
;;; repl - The repl needs to support several things
;;; 1. History and command line editing without using rlwrap.
;;; 2. Using Lisp wrapper functions for common Lisp commands
;;; 3. Implement a consistant mapping for other unix binaries
;;; 4. Have the ability to spawn a GUI environment based in a Firefox OS like approach
;;;

