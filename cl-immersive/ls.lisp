;;
;; ls - wrap the Unix ls command
;;
(defun ls (&optional (path-filter "*.*"))
  "Args: path-filter provides for the path regex you'd use with ls.
  Other options to be added later"
  (princ (shell (concatenate 'string "ls --color -la " 
			     (symbol-name-to-lowercase-string path-filter)))))

