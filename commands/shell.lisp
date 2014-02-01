;;
;; Define a Clisp like Shell command.
;;


(in-package :immersive)

(defun shell (&optional (shell_cmd "$SHELL"))
  "shell - send a command to the system shell (e.g. Bash).

  SHELL calls the EXT:SYSTEM function. Executes SHELL_CMD if given, otherwise
  User sub-shell is spawned. SHELL_CMD be string or symbol, 256 characters max.
  
  Args: the command to send to the shell as a string.
  
  Returns: t if successful, nil otherwise."
  #+ecl
  (eq 0 (ext:system shell_cmd))
  #+ccl (princ "shell not implemented."))

;;
;; Quick shortcut to escape to bash.
;;
(defun bash ()
  "Forks to a Bash shell.
  Returns T if successful, nil otherwise"
  (eq 0 (shell "bash")))

