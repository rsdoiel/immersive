;;
;; ls - wrap the Unix ls command
;;
(defun ls (&optionall (:filter "*.*"))
  "Args: filter provides for the path regex you'd use with ls.
  Other options to be added later"
  (princ (shell (concatenate 'string "ls -la " :filter))))

