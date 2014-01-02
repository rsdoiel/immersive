;;;
;;; git-scm.lisp - quick and dirty wrapper for git to use from 
;;; _ecl_ lisp.
;;;
;;; Author: R. S. Doiel, <rsdoiel@yahoo.com>
;;;

;;
;; git-scm - wrap the basic git command.
;;
(defun git-scm (action &optional (arg1 nil) (arg2 nil) (arg3 nil) (arg4 nil) (arg5 nil))
  (let ((cmd nil))
    (setq action (symbol-name-to-lowercase-string action))

    ;;FIXME use variable lisp parameters instead of arg1, arg2, arg3, ...
    (if (not arg1)
      (setq arg1 " "))
    (setq arg1 (symbol-name-to-lowercase-string arg1))

    (if (not arg2)
      (setq arg2 " "))
    (setq arg2 (symbol-name-to-lowercase-string arg2))

    (if (not arg3)
      (setq arg3 " "))
    (setq arg3 (symbol-name-to-lowercase-string arg3))

    (if (not arg4)
      (setq arg4 " "))
    (setq arg4 (symbol-name-to-lowercase-string arg4))

    (if (not arg5)
      (setq arg5 " "))
    (setq arg5 (symbol-name-to-lowercase-string arg5))


    (setq cmd (string-trim " "
			   (concatenate 'string "git " 
					action " " 
					arg1 " " 
					arg2 " " 
					arg3 " " 
					arg4 " " 
					arg5)))
    (princ cmd)
    (if (> (ext:system cmd) 0) () t)))


;;
;; git-status - run git status on the repo.
;;
(defun git-status ()
  (git-scm "status"))

;;
;; git-add - Add a file to the git repo.
;;
(defun git-add (fname)
  (git-scm "add" (system-name-to-lowercase-string fname)))

;;
;; git-commit - Commit the current state of development.
;;
(defun git-commit (&optional (msg "snapshot"))
  (git-scm "commit -am" 
	   (concatenate 'string "\"" 
			(symbol-name-to-lowercase-string msg) "\"")))

;;
;; git-push - Push the current state to master
;;
(defun git-push (&optional (source " ") (branch " "))
  (git-scm "push" (symbol-name-to-lowercase-string source)
	   (symbol-name-to-lowercase-string branch)))

;;
;; git-pull - Pull the current state from master
;;
(defun git-pull (&optional (source " ") (branch " "))
  (git-scm "pull" source branch))

;;
;; git-pull-origin-master - short cut to execute git pull origin master
;;
(defun git-pull-origin-master () (git-pull "origin" "master"))

;;
;; git-push-orgin-master - shortgut to push to origin master
(defun git-push-origin-master () (git-push "origin" "master"))

;;
;; git-clone - clone a remote repo
;;
(defun git-clone (repos-url) 
  (git-scm "clone " 
	   (concatenate 'string "\""
			(symbol-name-to-lowercase-string repos-url) "\"")))

