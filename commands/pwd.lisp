;;
;; Get working directory
;;

(defun pwd ()
  #+sbcl
  (sb-posix:getcwd)
  #+ecl
  (ext:getcwd)
  #+ccl
  (ccl::current-directory-name))

