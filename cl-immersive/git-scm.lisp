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
    (if (eq (type-of action) 'SYMBOL)
      (setq action (string-downcase (symbol-name action))))

    ;;FIXME use variable lisp parameters instead of arg1, arg2, arg3, ...
    (if (not arg1)
      (setq arg1 " "))
    (if (eq (type-of arg1) 'SYMBOL)
      (setq arg1 (string-downcase (symbol-name arg1))))

    (if (not arg2)
      (setq arg2 " "))
    (if (eq (type-of arg2) 'SYMBOL)
      (setq arg2 (string-downcase (symbol-name arg2))))

    (if (not arg3)
      (setq arg3 " "))
    (if (eq (type-of arg3) 'SYMBOL)
      (setq arg3 (string-downcase (symbol-name arg3))))

    (if (not arg4)
      (setq arg4 " "))
    (if (eq (type-of arg4) 'SYMBOL)
      (setq arg4 (string-downcase (symbol-name arg4))))

    (if (not arg5)
      (setq arg5 " "))
    (if (eq (type-of arg5) 'SYMBOL)
      (setq arg5 (string-downcase (symbol-name arg5))))


    (setq cmd (string-trim " " (concatenate 'string "git " action " " arg1 " " arg2 " " arg3 " " arg4 " " arg5)))
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
  (git-scm "add" fname))

;;
;; git-commit - Commit the current state of development.
;;
(defun git-commit (&optional (msg "snapshot"))
  (git-scm "commit -am" (concatenate 'string "\""  msg "\"")))

;;
;; git-push - Push the current state to master
;;
(defun git-push (&optional (source " ") (branch " "))
  (git-scm "push" source branch)) 

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
  (git-scm "clone " (concatenate 'string "\""  repos-url "\"")))

