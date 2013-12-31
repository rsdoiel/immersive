;;;;
;;;; immersive.lisp - a light wrapper around the Unix environment to help
;;;; create an immersive Lisp experience.
;;;;
;;;; Author: R. Doiel, <rsdoiel@gmail.com>
;;;; copyright (c) 2013
;;;; Released under the BSD 2 clause license
;;;; See http://opensource.org/licenses/BSD-2-Clause for details.
;;;;

;;;
;;; immersive the command that loads the immersive lisp environment.
;;;
(defun immersive (&optional (intention "load"))
  "immersive loads the immersive Lisp environment from $HOME directory
  of the logged in individual.
  Args: intention - you can default it loads either a .fas or .lisp files found
  in $HOME/cl-immersive
  
  Side effects: Adds a the global space various symbols and functions.
  Returns the results of executing $HOME/cl-immersive/setup.lisp."

  (let ((target (concatenate 'string (ext:getenv "HOME") "/cl-immersive/setup"))
	(target-lisp (concatenate 'string (ext:getenv "HOME") "/cl-immersive/setup.lisp")))
    (if (string= intention "load")
      (load target))
    (if (string= intention "lisp")
      (load target-lisp))
    (if (string= intention "compile")
      (progn
	(compile-file target-lisp)
	(load target)))
    (cl-immersive-setup intention)))
