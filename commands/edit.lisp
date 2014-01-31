;;;
;;; edit - wrap the Lisp ED function to use lowercase strings when converted
;;; symbols to strings.
;;; update-ed - edit a file then load it. Remember the last file worked on.
;;;
(in-package :immersive)

;; set a default value for *update-ed-filename* without overwriting it if
;; already defined.
(defvar *update-edit* "untitled.lisp")

(defun edit (filename)
  "edit - wrap the Lisp ED function but default to downcase filename
  when filenames are symbols.
  Args: filename
  Returns: value of Lisp ED function."
  (ed (convert-symbol-or-pathname filename)))

(defun update-edit (&optional (filename nil))
  "update-edit - edit then load a filename.
  Args: filename (optional), defaults to nil (i.e. use the last value of *update-edit*)
  Side effects: filename if not nil will update update the value of *update-edit*.
  Returns: the results of load."
  (progn
    (if filename
      (setq *update-edit* (convert-symbol-or-pathname filename)))
    (ed *update-edit*)
    (load *update-edit*)))

