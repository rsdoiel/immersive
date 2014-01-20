;;;
;;; Immersive Dev tools
;;;

;;
;; Edit an file then load the file into the REPL
;;
(defun edit-then-load (fname)
  (progn
    (ed fname)
    (load fname)))

;;
;; update-tools - short cut to edit then load immersive-dev-tools.lisp
;;
(defun update-tools ()
  (edit-then-load "dev-tools.lisp"))

;;
;; update-engine - short cut to editor engine code and then load the result
;;
(defun update-engine (&optional (fname "game-engine.lisp"))
  (edit-then-load fname))

;;
;; udpate-data - short cut to edit game data then load the result
;;
(defun update-data (&optional (fname "wizards-cottage.lisp") (engine-name "game-engine.lisp"))
  (progn
    (load engine-name)
    (edit-then-load fname)
    (start-engine fname)))


;;
;; update-tests - short cude to edit the test data and automated tests.
;;
(defun update-tests (&optional (fname "game-engine-tests.lisp"))
  (edit-then-load fname))


