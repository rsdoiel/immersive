;;;;
;;;; Wrap the Unix rmdir command
;;;;

(defun rmdir (folder-name)
  "rmdir is a wrapper of the Unix remove directory command.

  Args: pathname
 
  + folder-name is the path (relative or full) the directory you want to create.

  Side effects: Changes the working directory."
  #+sbcl
  (sb-posix:rmdir `(.folder-name))
  #+ecl
  (ext:run-program "rmdir" `(,folder-name))
  #+ccl
  (ccl:run-program "rmdir" `(,folder-name)))

