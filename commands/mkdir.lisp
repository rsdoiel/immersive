;;;;
;;;; Wrap the Unix mkdir command
;;;;

(in-package :immersive)

(defun mkdir (folder-name &optional (parent nil))
  "mkdir is a wrapper of the Unix make directory command. Without parameters
  it will change the working directory to the value of $HOME.

  Args: pathname, parent (optional, default is nil)
 
  + folder-name is the path (relative or full) the directory you want to create.
  + parent if true will attempt to create the parent directories as requested by
    pathname

  Returns: The new working directory location as a pathname."
    (if parent
      (progn
	#+ecl
	(ext:run-program "mkdir" `("-p" ,folder-name)
			 :output t :error t)
	#+ccl
	(ccl:run-program "mkdir" `("-p" ,folder-name)
			 :output t :error t))
      (progn
	#+ecl
	(ext:run-program "mkdir" `(,folder-name)
			 :output t :error t)
	#+ccl
	(ccl:run-program "mkdir" `(,folder-name)
			 :output t :error t))))

