;;
;; Change working directory
;;
(defun cd (&optional (pathname (si:getenv "HOME")))
  "Args: (%optional pathname)
  CD calls ext:chdir to change the working directory. CD without args should
  change the directory back to your $HOME location."
  (progn
    (ext:chdir pathname)
    (ext:getcwd)))

