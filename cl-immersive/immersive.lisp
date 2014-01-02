;;;;
;;;; immersive.lisp - bootstrap our Immersive Lisp environment for
;;;; the Raspberry Pi.
;;;;
;;;; Author: R. Doiel, <rsdoiel@gmail.com>
;;;; copyright (c) 2013
;;;; Released under the BSD 2 clause license
;;;; See http://opensource.org/licenses/BSD-2-Clause for details.
;;;;


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

;; Convert a pathname type to a string type (optionally minus the filename extention),
;; #P and the quotes.
(defun pathname-to-string (filename-as-pathname &optional (extname nil))
  "Converts a pathname to a string with or without the filename's file extension.
  Args: filename-as-pathname, extname (defaults to nil)
     
    + filename-as-pathname would be the type returned for (directory ...)
    + extname would be a string such as _.lisp_ including the leading period or nil
    if the extention needs to remain untouched.

   Returns a string version of the pathname."
  (let ((fname-string nil))
    (progn
      (setq fname-string (format nil "~S" 
				 (symbol-name-to-lowercase-string filename-as-pathname)))
      (if extname
	(subseq fname-string 3 (- (length fname-string) (1+ (length extname))))
	(subseq fname-string 3 (1- (length fname-string)))))))

;;
;; Load or Compile then load a file based on an intention.
;; The intentions understood are to load what's available
;; preferring the .fas file, explicity load the .lisp version
;; or compile the .lisp version and load the resulting .fas file.
;;
(defun smart-load (fname-string intention)
  "Args: fname-string, intention

    + fname-string would be a string version of a pathname *without a filename extention*.
    + intention is a string of either *compile*, *lisp* or *load*.

  Returns the results of (load ...)"
    (progn
      (if (string= intention "compile")
	(compile-file (concatenate 'string 
				   (symbol-name-to-lowercase-string fname-string) ".lisp")))
      (if (string= intention "lisp")
	  (load (concatenate 'string 
			     (symbol-name-to-lowercase-string fname-string) ".lisp"))
	  (load (symbol-name-to-lowercase-string fname-string)))))

;;
;; Scan this immersive folder and load the lisp or fas files we find.
;; If requested compile them first.
;;
(defun immersive-init (&optional (intention "load"))
  "Args: intention is a string that is either load, lisp, compile. The default is load.
  	load - will load either .fas or if not found the .lisp file
  	lisp - will explicitly load the .lisp files only
  	compile - will compile the lisp files and then load the .fas rendered.
  Returns results of loadnig the list of files."
  (let ((fname-as-string nil))
    ; Now process all the parts, make a list of files and compile or load them
    (dolist (filename-as-pathname (directory (concatenate 'string (ext:getenv "HOME") "/cl-immersive/*.lisp")))
      (setq fname-as-string (pathname-to-string filename-as-pathname ".lisp"))
      (if (equal (search "/init" fname-as-string) ()) 
	(smart-load fname-as-string intention)))))

;;;
;;; immersive the command that loads the immersive lisp environment.
;;;
(defun immersive (&optional (intention "load"))
  "immersive loads the immersive Lisp environment from $HOME directory
  of the logged in individual.
  Args: intention - you can default it loads either a .fas or .lisp files found
  in $HOME/cl-immersive
  
  Side effects: Adds a the global space various symbols and functions.
  Returns the results of executing $HOME/cl-immersive/init.lisp."

  (let ((target (concatenate 'string (ext:getenv "HOME") "/cl-immersive/init"))
	(target-lisp (concatenate 'string (ext:getenv "HOME") "/cl-immersive/init.lisp")))
    (if (string= intention "load")
      (load target))
    (if (string= intention "lisp")
      (load target-lisp))
    (if (string= intention "compile")
      (progn
	(compile-file target-lisp)
	(load target)))
    (immersive-init intention)))
