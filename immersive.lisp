;;;;
;;;; immersive.lisp - bootstrap our Immersive Lisp environment for
;;;; the Raspberry Pi.
;;;;
;;;; Author: R. Doiel, <rsdoiel@gmail.com>
;;;; copyright (c) 2013
;;;; Released under the BSD 2 clause license
;;;; See http://opensource.org/licenses/BSD-2-Clause for details.
;;;;

;;
;; convert-symbol-or-pathname - converts a SYMBOL into a lowercase
;; string or a PATHNAME into a string. This makes it easy to 
;; override the default Lisp symbol to string conversion to 
;; uppercase.
;;
(defun convert-symbol-or-pathname (symbol-or-pathname)
  "Converts a symbol to a lower case string and a pathname to string.
  If it already is a string no change is made.
  
  Args: symbol-or-pathname - the symbol or pathname you wish to process.
  Returns: a string."
  (cond 
    ((eq (type-of symbol-or-pathname) 'SYMBOL)
     (string-downcase (symbol-name symbol-or-pathname)))
    ((eq (type-of symbol-or-pathname) 'PATHNAME) 
     (let ((fname-string (format nil "~S" symbol-or-pathname)))
       (subseq fname-string 3 (1- (length fname-string)))))
    (t symbol-or-pathname)))

;;
;; Load or Compile then load a file based on an intention.
;; The intentions understood are to load what's available
;; preferring the .fas file, explicity load the .lisp version
;; or compile the .lisp version and load the resulting .fas file.
;;
(defun smart-load (filename intention)
  "Args: filename, intention

    + filename would be a string, SYMBOL or PATHNAME.
    + intention is a string of either *compile*, *lisp* or *load*.

  Returns the results of (load ...)"
    (let ((fname-no-ext nil)
	  (fname nil))
      (setq fname (convert-symbol-or-pathname filename))
      (if (search ".lisp" fname)
	(setq fname-no-ext (subseq fname 0 (- (length fname) 5)))
	(progn
	  (setq fname-no-ext fname)
	  (setq fname (concatenate 'string fname-no-ext ".lisp"))))

      (if (string= intention "compile")
	(compile-file fname)) 
      (if (string= intention "lisp")
	  (load fname)
	  (load fname-no-ext))
      ))

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
    default))

;;
;; Scan this immersive folder and load the lisp or fas files we find.
;; If requested compile them first.
;;
(defun immersive (&optional (intention "load"))
  "immersive loads the immersive Lisp environment from $HOME directory
  of the logged in individual.

  Args: intention is a string that is either load, lisp, compile. 
  The default is load.
  	load - will load either .fas or if not found the .lisp file
  	lisp - will explicitly load the .lisp files only
  	compile - will compile the lisp files and then load the .fas rendered.

  Side effects: Adds a various symbols and functions to the user space.

  Returns results of loading the list of files."
  ; Now process all the parts, make a list of files and compile or load them
  (dolist (filename 
	      (directory (concatenate 
			   'string (get-os-environment "HOME") "/immersive/commands/*.lisp")))
    (let ((fname (convert-symbol-or-pathname filename)))
      (if (not (search "/immersive.lisp" fname))
	  (smart-load fname intention)))))

