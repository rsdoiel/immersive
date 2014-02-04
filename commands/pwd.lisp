;;
;; Get working directory
;;

(in-package :immersive)

(defun pwd ()
  #+sbcl
  (error "pwd not implemented yet.")
  #+ecl
  (ext:getcwd)
  #+ccl
  (ccl::current-directory-name))

