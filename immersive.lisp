;;;;
;;;; immersive.lisp - bootstrap our Immersive Lisp environment for
;;;; the Raspberry Pi.
;;;;
;;;; Author: R. Doiel, <rsdoiel@gmail.com>
;;;; copyright (c) 2013
;;;; Released under the BSD 2 clause license
;;;; See http://opensource.org/licenses/BSD-2-Clause for details.
;;;;
(in-package #:immersive)

;;
;; immersive-getenv - get the Lisp's environment variable. This is Lisp dependant even in CL.
;; this was taken from http://cl-cookbook.sourceforge.net/os.html
;;
(defun get-os-environment (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
		  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
    #+Allegro (sys:getenv name)
    #+CLISP (ext:getenv name)
    #+ECL (si:getenv name)
    #+SBCL (sb-unix::posix-getenv name)
    #+LISPWORKS (lispworks:environment-variable name)
    #+CCL (ccl:getenv name)
    #+CLOZURE (ccl:getenv name)
    default))

