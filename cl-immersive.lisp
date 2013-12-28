;;
;; cl-immersive.lisp - setup an immersive Lisp environment for Raspberry Pi
;; Hosted by the [ecl]() Lisp implementation, cl-immersive is able to leverage
;; Most prior work done in Unix and still provide a rich Lisp environment to
;; build application and explore your Raspberry Pi.
;;

(defun cl-immersive (&optional (action "load"))
  "Args: action - can be load, lisp or compile
  Eventually this will allow us to optimise what happens easily from .eclrc"
  (progn
    (or (equal action "load") (load "cl-immersive/setup.lisp"))
    (or (equal action "lisp") (load "cl-immersive/setup"))
    (or (equal action "compile") (compile-file "cl-immersive/setup.lisp"))))

