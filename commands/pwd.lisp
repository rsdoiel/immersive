;;
;; Get working directory
;;

(in-package :immersive)

(defun pwd ()
  #+ecl
  (ext:getcwd)
  #+ccl
  (ccl::current-directory-name))

