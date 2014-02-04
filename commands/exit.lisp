(in-package :immersive)

(defun exit (&optional (exit_code 0))
  "Args: (&optional exit_code)
  Exit the ecl shell"
  #+sbcl
  (error "use (sb-ext:exit) instead.")
  #+ecl
  (ext:quit exit_code)
  #+ccl (ccl:quit))

