(in-package :immersive)

(defun exit (&optional (exit_code 0))
  "Args: (&optional exit_code)
  Exit the ecl shell"
  #+ecl
  (ext:quit exit_code)
  #+ccl (ccl:quit))

