;;
;; ls - wrap the Unix ls command
;;

(in-package :immersive)

(defun ls (&optional (filter "./"))
  "list the contents of a directory.
  Args: filter is a Unix path string can include wild cards.
  Returns: A list of files and directories as pathnames"
  (let ((path-type (if (string= (subseq filter 0 1) "/")
		     ':absolute
		     ':relative))
        (base-path (pathname-directory (pathname filter)))
        (base-name (pathname-name (pathname filter)))
        (base-type (pathname-type (pathname filter))))
    (if base-type
      (directory (make-pathname :directory base-path :name base-name :type base-type))
      (progn
        (if (and (not base-path) (not base-type))
          (setq base-path (pathname-directory
                            (pathname (concatenate 'string filter "/")))))
        (concatenate 'list
                     (directory (make-pathname :directory 
                                               (append base-path '(:wild))))
                     (directory (make-pathname :directory base-path
                                               :name "*" :type "*")))))))


;;
;; Need something like stat but since the "disc" is the web
;; and other immersive lisp pi deployments we need additional
;; info. Meta data should probably be something like
;; + owner
;; + group
;; + permissions
;; + hostname
;; + pathname/filename
;; + extension
;; + mime-type
;; + protocol to access it (e.g. ssh://, sftp://, https://, git://)
;;
