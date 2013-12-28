;;
;; Scan this path and load the lisp or fas file we find.
;;
(defun cl-immersive-pathname-to-string (filename-as-pathname)
  (let ((fname-string nil))
    (progn
      (setq fname-string (format nil "~S" filename-as-pathname))
      (subseq fname-string 3 (- (length fname-string) 6)))))

(defun cl-immersive-smart-load (filename-as-pathname)
  (let ((fname-string (cl-immersive-pathname-to-string filename-as-pathname)))
      (load fname-string)))

; Scan the directory and get a list of filenames
(defun cl-immersive-setup ()
  ; With list of filenames load, trim .lisp and them
  (dolist (filename-as-pathname (directory (concatenate 'string (ext:getenv "HOME") "/cl-immersive/*.lisp")))
    (cl-immersive-smart-load filename-as-pathname)))

