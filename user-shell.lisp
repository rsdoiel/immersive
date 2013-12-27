;;
;; Define a Clisp like Shell command.
;;
(defun shell (&optional (shell_cmd "$SHELL"))
"Args: (&optional shell_cmd)
SHELL calls the EXT:SYSTEM function. Executes SHELL_CMD if given, otherwise
User sub-shell is spawned. SHELL_CMD be string or symbol, 256 characters max."
   (ext:system shell_cmd))

;;
;; Quick shortcut to escape to bash.
;;
(defun bash ()
	(shell "bash"))

;;
;; Enabled readline
;;

