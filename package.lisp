;;;; package.lisp

(defpackage #:immersive
  (:use #:cl)
  (:export :cd :pwd :ls :update-edit 
	   :edit :mkdir :rmdir
	   :git-status :git-add
	   :git-commit :git-push
	   :git-pull :git-push-origin-master
	   :git-scm
	   :cat))

