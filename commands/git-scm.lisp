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
    (setq action (convert-symbol-or-pathname action))

    ;;FIXME use variable lisp parameters instead of arg1, arg2, arg3, ...
    (if (not arg1)
      (setq arg1 " "))
    (setq arg1 (convert-symbol-or-pathname arg1))

    (if (not arg2)
      (setq arg2 " "))
    (setq arg2 (convert-symbol-or-pathname arg2))

    (if (not arg3)
      (setq arg3 " "))
    (setq arg3 (convert-symbol-or-pathname arg3))

    (if (not arg4)
      (setq arg4 " "))
    (setq arg4 (convert-symbol-or-pathname arg4))

    (if (not arg5)
      (setq arg5 " "))
    (setq arg5 (convert-symbol-or-pathname arg5))


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
  (git-scm "add" (convert-symbol-or-pathname fname)))

;;
;; git-commit - Commit the current state of development.
;;
(defun git-commit (&optional (msg "snapshot"))
  "Execute git commit -am MSG

  Args: msg - a string message to include on the commit.
  Returns status of commit"
  (git-scm "commit -am" 
	   (concatenate 'string "\"" msg "\"")))

;;
;; git-push - Push the current state to master
;;
(defun git-push (&optional (source " ") (branch " "))
  "Execute a git push with optional source and branch references.
  
  Args: source - a string, branch - a string"
  (git-scm "push" source branch))

;;
;; git-pull - Pull the current state from master
;;
(defun git-pull (&optional (source " ") (branch " "))
  (git-scm "pull" source branch))

;;
;; git-pull-origin-master - short cut to execute git pull origin 
;; master
;;
(defun git-pull-origin-master () (git-pull "origin" "master"))

;;
;; git-push-orgin-master - shortcut to push to origin master
;;
(defun git-push-origin-master () (git-push "origin" "master"))

;;
;; git-clone - clone a remote repo
;;
(defun git-clone (repos-url) 
  "Execute git clone for a git URL.

  Args: repos-url as a string"
  (git-scm "clone " 
	   (concatenate 'string "\"" repos-url "\"")))

;;
;; git-mv - shutcut for: git mv OLDNAME NEWNAME
;;
(defun git-mv (old-name new-name)
  (git-scm "mv " 
	   (concatenate 'string "\"" old-name "\"")
	   (concatenate 'string "\"" new-name "\"")))
		   
