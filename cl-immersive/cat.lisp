;;;;
;;;; cat.lisp - wrap Unix cat command.
;;;;
(defun cat (filename)
  (shell (concatenate 'string "cat " (symbol-name-to-lowercase-string filename))))
