;;;
;;; git-cmds.lisp - quick and dirty wrapper for git to use from 
;;; _ecl_ lisp.
;;;
;;; Author: R. S. Doiel, <rsdoiel@yahoo.com>
;;;

;;
;; git-cmds - wrap the basic git command.
;;
(defun git-cmds (action &optional (arg1 " ") (arg2 " ") (arg3 " ") (arg4 " ") (arg5 " "))
  (let ((cmd nil))
    ;FIXME: turn a symbol into a string for the 'action' element.
    (setq cmd (string-trim " " (concatenate 'string "git " action " " arg1 " " arg2 " " arg3 " " arg4 " " arg5)))
    (princ cmd)
    (ext:system cmd)
    ))


;;
;; git-status - run git status on the repo.
;;
(defun git-status ()
  (git-cmds "status"))

;;
;; git-add - Add a file to the git repo.
;;
(defun git-add (fname)
  (git-cmds "add" fname))

;;
;; git-commit - Commit the current state of development.
;;
(defun git-commit (&optional (msg "snapshot"))
  (git-cmds "commit -am" (concatenate 'string "\""  msg "\"")))

;;
;; git-push - Push the current state to master
;;
(defun git-push (&optional (source "origin") (branch "master"))
  (git-cmds "push" source branch)) 

;;
;; git-pull - Pull the current state from master
;;
(defun git-pull (&optional (source "origin") (branch "master"))
  (git-cmds "pull" source branch))

