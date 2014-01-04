;;;;
;;;; cat.lisp - wrap Unix cat command.
;;;;
(defun cat (filename)
  (shell (concatenate 'string "cat " (convert-symbol-or-pathname filename))))
