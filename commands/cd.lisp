;;
;; Change working directory
;;


;FIXME: Need to make sure conert-symbol-or-pathname is
; available from immersive.lisp, probably need some sort of 
; require, export, import or something...

(defun cd (&optional (pathname (get-os-environment "HOME")))
  "cd is a wrapper of the Unix change directory command. Without parameters
  it will change the working directory to the value of $HOME.

  Args: pathname - is either a string (for explicit Unix path) or symbol 
  (converted to lowercase and applied as a Unix path).
  Side effects: Changes the working directory.
  Returns: The new working directory location as a pathname."
  #+clisp
    (ext:cd pathname)
  #+sbcl
    (sb-posix:chdir pathname)
  #+ecl 
    (progn (ext:chdir pathname) (ext:getcwd))
  #+ccl 
    (progn (ccl::cd pathname) (ccl::current-directory-name)))

