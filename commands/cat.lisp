;;;;
;;;; cat.lisp - wrap Unix cat command.
;;;;

;;;
;;; echo-file-as-string read in an entire file and return as a string.
;;;
(defun echo-file-as-string (filename)
  "echo-file - opens a file stream and returns the contents as a
  single string.
  Args: filename - a pathname or string reference to a file.
  Returns: a string of contents of file or single new line."
  (if (probe-file filename)
    (with-open-file (stream filename)
      (let ((seq (make-array (file-length stream)
                             :element-type 'character 
                             :fill-pointer t)))
        (setf (fill-pointer seq) (read-sequence seq stream)) seq))
    #\NewLine)) 

(defun cat (&rest filenames)
  (let ((text-list ()))
    (loop
      for fname in filenames
      do (setq text-list
               (concatenate 'string text-list 
                            (echo-file-as-string fname))))
    text-list))
