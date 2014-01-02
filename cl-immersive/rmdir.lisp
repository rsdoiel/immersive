;;;;
;;;; Wrap the Unix rmdir command
;;;;
(defun rmdir (pathname)
  "rmdir is a wrapper of the Unix remove directory command.

  Args: pathname
 
  + pathname is the path (relative or full) the directory you want to create.

  Side effects: Changes the working directory."
  (shell (concatenate 'string "rmdir "
		      (symbol-name-to-lowercase-string pathname))))

