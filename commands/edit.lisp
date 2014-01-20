;;;
;;; edit - wrap the Lisp ED function to use lowercase strings when converted
;;; symbols to strings.
;;; update-ed - edit a file then load it. Remember the last file worked on.
;;;

;; set a default value for *update-ed-filename* without overwriting it if
;; already defined.
(defvar *update-ed* "untitled.lisp")

(defun edit (filename)
  "edit - wrap the Lisp ED function but default to downcase filename
  when filenames are symbols.
  Args: filename
  Returns: value of Lisp ED function."
  (ed (convert-symbol-or-pathname filename)))

(defun update-ed (&optional (filename nil))
  "update-ed - edit then load a filename.
  Args: filename (optional), defaults to nil (i.e. use the last value of *update-ed*)
  Side effects: filename if not nil will update update the value of *update-ed*.
  Returns: the results of load."
  (progn
    (if filename
      (setq *update-ed* (convert-symbol-or-pathname filename)))
    (ed *update-ed*)
    (load *update-ed*)))

