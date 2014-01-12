;;
;; ls - wrap the Unix ls command
;;
(defun ls (&optional (folder ".") (name "*") (type "*"))
  "list the contents of a directory.
  Args: folder [absolute or relative path], name [base name of file], type [filename extension, e.g. lisp]
  Returns: A list of files and directories"
  (let ((path-type (if (string= (subseq folder 0 1) "/")
		     ':absolute
		     ':relative)))
    (concatenate 'list (if (string= "*" type) (directory (make-pathname :directory `(,path-type ,folder :wild))))
		 (directory (make-pathname :directory `(,path-type ,folder) :name name :type type)))))

