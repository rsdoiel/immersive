;;;;
;;;; update-ed - edit a file then load it. Remember the last file worked on.
;;;;

;; set a default value for *update-ed-filename* without overwriting it if
;; already defined.
(defvar *update-ed* "untitled.lisp")

(defun update-ed (&optional (filename nil))
  "update-ed - edit then load a filename.
  Args: filename (optional), defaults to nil (i.e. use the last value of *update-ed*)
  Side effects: filename if not nil will update update the value of *update-ed*.
  Returns: the results of load."
  (progn
    (if (eq (type-of filename) 'SYMBOL)
      (setq filename (string-downcase (symbol-name filename))))
    (if filename
	(setq *update-ed* filename))
    (format t "editting ~S" *update-ed*)
    (ed *update-ed*)
    (load *update-ed*)))

