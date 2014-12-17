;;
;; Define a Clisp like Shell command.
;;


;;
;; Quick shortcut to escape to bash.
;;
(defun bash ()
  "Forks to a Bash shell.
  Returns T if successful, nil otherwise"
  #+sbcl
  (error "bash not available in SBCL")
  #+ecl
  (ext:system "bash")
  #+ccl
  (error "bash not available in CCL."))

