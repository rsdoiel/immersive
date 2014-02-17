;;;;
;;;; Wrap the Unix rmdir command
;;;;

(in-package :immersive)


(defun rmdir (folder-name)
  "rmdir is a wrapper of the Unix remove directory command.

  Args: pathname
 
  + folder-name is the path (relative or full) the directory you want to create.

  Side effects: Changes the working directory."
  #+sbcl
  (error "rmdir not implemented yet.")
  #+ecl
  (ext:run-program "rmdir" `(,folder-name))
  #+ccl
  (ccl:run-program "rmdir" `(,folder-name)))

