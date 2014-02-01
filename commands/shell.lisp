;;
;; Define a Clisp like Shell command.
;;


(in-package :immersive)

;;
;; Quick shortcut to escape to bash.
;;
(defun bash ()
  "Forks to a Bash shell.
  Returns T if successful, nil otherwise"
  #+ecl
  (ext:system "bash")
  #+ccl
  (error "bash not available in CCL."))

