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
;; Change working directory
;;
(defun cd (&optional (pathname (si:getenv "HOME")))
  "Args: (%optional pathname)
  CD calls ext:chdir to change the working directory. CD without args should
  change the directory back to your $HOME location."
  (progn
    (ext:chdir pathname)
    (ext:getcwd)))


;;
;; Get working directory
;;
(defun pwd ()
  (ext:getcwd))

;;
;; make a directory
;;
(defun mkdir (pathname &optional (p nil))
  "Args: pathname [p]
  MKDIR creates a new directory if P is true then it will create any missing
  directories listed in the path."
  (princ (concatenate 'string "DEBUG pathname: " pathname)))


;;
;; exit the shell, alias for (quit 0)
;;
(defun exit (&optional (exit_code 0))
  "Args: (&optional exit_code)
  Exit the ecl shell"
  (quit exit_code))

