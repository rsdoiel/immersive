;;;
;;; cl-immserive/setup.lisp - bootstrap my immersive lisp environment for 
;;; _ecl_ and _Raspbery Pi_
;;;
;;; Setup an immersive Lisp environment for Raspberry Pi
;;; Hosted by the [ecl]() Lisp implementation, cl-immersive will be able to leverage
;;; Most prior work done in Unix and still provide a rich Lisp environment to
;;; build application and explore your Raspberry Pi.
;;;


;; Convert a pathname type to a string type (optionally minus the filename extention),
;; #P and the quotes.
(defun cl-immersive-pathname-to-string (filename-as-pathname &optional (extname nil))
  "Args: filename-as-pathname, extname (defaults to nil)
     
    + filename-as-pathname would be the type returned for (directory ...)
    + extname would be a string such as _.lisp_ including the leading period or nil
    if the extention needs to remain untouched.

   Returns a string version of the pathname."
  (let ((fname-string nil))
    (progn
      (setq fname-string (format nil "~S" filename-as-pathname))
      (if extname
	(subseq fname-string 3 (- (length fname-string) (1+ (length extname))))
	(subseq fname-string 3 (1- (length fname-string)))))))

;;
;; Load or Compile then load a file based on an intention.
;; The intentions understood are to load what's available
;; preferring the .fas file, explicity load the .lisp version
;; or compile the .lisp version and load the resulting .fas file.
;;
(defun cl-immersive-smart-load (fname-string intention)
  "Args: fname-string, intention

    + fname-string would be a string version of a pathname *without a filename extention*.
    + intention is a string of either "compile", "lisp" or "load".

  Returns the results of (load ...)"
    (progn
      (or (equal intention "compile") (compile-file fname-string))
      (if (equal intention "lisp")
	(load (concatenate 'string fname-string ".lisp"))
	;; defautl to load either .fas or .lisp as available
	(load fname-string))))

;;
;; Scan this cl-immersive folder and load the lisp or fas files we find.
;; If requested compile them first.
;;
(defun cl-immersive-setup (&optional (action "load"))
  "Args: action is a string that is either load, lisp, compile. The default is load.
  	load - will load either .fas or if not found the .lisp file
  	lisp - will explicitly load the .lisp files only
  	compile - will compile the lisp files and then load the .fas rendered.
  Returns results of loadnig the list of files."
  (let ((fname-as-string nil))
    ; Now process all the parts, make a list of files and compile or load them
    (dolist (filename-as-pathname (directory (concatenate 'string (ext:getenv "HOME" "/cl-immersive/*.lisp"))))
      (setq fname-as-string (cl-immersive-pathname-to-string filename-as-pathname ".lisp"))
      ;FIXME: don't bother loading with setup.lisp
      (format t "DEBUG filename-as-string ~S~%" fname-as-string)
      (cl-immersive-smart-load fname-as-string action))))

