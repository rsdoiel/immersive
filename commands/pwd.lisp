;;
;; Get working directory
;;

(defun pwd ()
  #+sbcl
  (error "pwd not implemented yet.")
  #+ecl
  (ext:getcwd)
  #+ccl
  (ccl::current-directory-name))

