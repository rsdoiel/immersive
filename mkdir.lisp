;;;;
;;;; Wrap the Unix mkdir command
;;;;
(defun mkdir (pathname &optional (parent nil))
  "mkdir is a wrapper of the Unix make directory command. Without parameters
  it will change the working directory to the value of $HOME.

  Args: pathname, parent (optional, default is nil)
 
  + pathname is the path (relative or full) the directory you want to create.
  + parent if true will attempt to create the parent directories as requested by
    pathname

  Returns: The new working directory location as a pathname."
  (let ((path (convert-symbol-or-pathname pathname)))
    (eq 0 (if parent
	    (shell (concatenate 'string "mkdir -p " path))
	    (shell (concatenate 'string "mkdir " path))))))


