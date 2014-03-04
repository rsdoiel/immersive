;;;; package.lisp
(defpackage #:immersive
  (:use #:cl)
  (:export :pwd 
	   :cd
	   :mkdir 
	   :rmdir
	   :ls 
	   :cat 
	   :update-edit 
	   :edit 
	   :git-scm
	   :git-status 
	   :git-add
	   :git-commit 
	   :git-push
	   :git-pull 
	   :git-push-origin-master
	   ))
