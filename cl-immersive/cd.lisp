;;
;; Change working directory
;;
(defun cd (&optional (pathname (ext:getenv "HOME")))
  "cd is a wrapper of the Unix change directory command. Without parameters
  it will change the working directory to the value of $HOME.

  Args: pathname - is either a string (for explicit Unix path) or symbol 
  (converted to lowercase and applied as a Unix path).
  Side effects: Changes the working directory.
  Returns: The new working directory location as a pathname."
  (progn
    (ext:chdir (convert-symbol-or-pathname pathname))
    (ext:getcwd)))

