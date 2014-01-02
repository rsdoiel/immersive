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

;;;
;;; Common functions used in immersive Lisp
;;;

;;
;; symbol-name-to-lowercase-string - converts a symbol name into a lowercase
;; string. This makes it easy to override the default Lisp symbol to string
;; conversion to uppercase.
;;
(defun symbol-name-to-lowercase-string (symbol-or-string)
  "Converts a symbol name to a lower case string or leaves it alone
  if it already is a string.
  
  Args: symbol-or-string - the symbol or string you wish to process.
  Returns: a symbol converted to a lowercase string or leaves the string alone."
  (if (eq (type-of symbol-or-string) 'SYMBOL)
    (setq symbol-or-string 
	  (string-downcase (symbol-name symbol-or-string))))
  symbol-or-string)

