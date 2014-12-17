;;;;
;;;; ilsh - Adds Immersive commands to toplevel repl. This an experiment in creating an
;;;; hybrid Lisp/Unix experience.
;;;;

;;;
;;; Pull in quicklisp/asdf 3 libraries
;;;
(require "asdf")
(require "magic-ed")

;; Pull in the immersive collection of the commands.
(load "immersive.lisp")



