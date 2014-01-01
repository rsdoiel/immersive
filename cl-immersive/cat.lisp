;;;;
;;;; cat.lisp - wrap Unix cat command.
;;;;
(defun cat (filename)
  (if (eq (type-of filename) 'SYMBOL)
    (setq filename (string-downcase (symbol-name filename)))) 
  (shell (concatenate 'string "cat " filename)))
