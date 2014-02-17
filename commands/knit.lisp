;;;;
;;;; knit.lisp - my feable attempt to create a simple literate lisp environment
;;;; using Markdown containing Lisp codeblocks and a anchor element to indicate
;;;; what to do with the discovered code block. Think poor man's unsophisticated
;;;; tangle/cweb inspired by Donald Knuth's ideas.
;;;;

(in-package :immersive)

(defun knit (input-filename &optional (input-template "./") (output-directory "./"))
    "Knit takes an Markdown file as input and generates the appropriate HTML document
    and source code (e.g. Lisp) files.
    
    Args:
    input-filename - string version of the filename (e.g. my-litterate-program.md)
    
    Optional Args:
    input-template - the template used to wrap Markdown's HTML output
    output-directory - the directory where to write the output files
    
    Returns - list of files output"
    (princ "knit not implemented yet."))
