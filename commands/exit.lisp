(in-package :immersive)

(defun exit (&optional (exit_code 0))
  "Args: (&optional exit_code)
  Exit the ecl shell"
  (ext:quit exit_code))

