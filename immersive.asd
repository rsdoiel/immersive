;;;; immersive.asd

(asdf:defsystem #:immersive
  :serial t
  :name "immersive"
  :version "0.0.0"
  :description "An immersive Lisp environment intended to run on a Raspberry Pi Model B or A"
  :author "R. S. Doiel"
  :license "BSD 2-clause License"
  ;;;:depends-on (#:cl-fad
  ;;;             #:hunchentoot)
  :components ((:file "package")
               (:file "immersive")
               (:file "commands/cat")
               (:file "commands/cd")
               (:file "commands/edit")
               (:file "commands/git-scm")
               (:file "commands/knit")
               (:file "commands/ls")
               (:file "commands/mkdir")
               (:file "commands/pwd")
               (:file "commands/rmdir")
               (:file "commands/shell")))

