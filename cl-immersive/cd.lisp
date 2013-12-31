;;
;; Change working directory
;;
(defun cd (&optional (pathname (si:getenv "HOME")))
  "Args: (%optional pathname)
  CD calls ext:chdir to change the working directory. CD without args should
  change the directory back to your $HOME location."
  (progn
    (if (eq (type-of pathname) 'SYMBOL)
      (progn
	(print "turning symbol into string")
	(setq pathname (symbol-name pathname))
	(print pathname)
	(print (type-of pathname))
	(print "DEBUG now to find the right case..."))
      (progn
	(ext:chdir pathname)
	(ext:getcwd)))))

