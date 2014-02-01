;;;
;;; git-scm.lisp - quick and dirty wrapper for git to use from 
;;; _ecl_ lisp.
;;;
;;; Author: R. S. Doiel, <rsdoiel@yahoo.com>
;;;
(in-package :immersive)

;;
;; git-scm - wrap the basic git command.
;;
(defun git-scm (action &rest more-args)
  (let ((cmd (concatenate 'list `("git" ,action) more-args)))
    #+ecl
    (ext:run-program (first cmd) (rest cmd) :output t :error t)
    #+ccl
    (ccl:run-program (first cmd) (rest cmd) :output t :error t)
))

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
  "Execute git commit -am MSG

  Args: msg - a string message to include on the commit.
  Returns status of commit"
  (git-scm "commit" "-am" msg))  

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

