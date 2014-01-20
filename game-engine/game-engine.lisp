;;;;
;;;; game-engine is a simple Text Game Engine inspired by Conrad Barski's
;;;; book Land of Lisp Chapters 5 & 6.  It extends the concepts to include
;;;; additional features such as saving/loading game state, addition
;;;; vocabulary and the ability to load different stories based on
;;;; a common data structure suggested in the book.
;;;;
;;;; Author: R. S. Doiel, <rsdoiel@yahoo.com>
;;;; copyright (c) 2013 all rights reserved
;;;; released under the BSD 2-clause license
;;;; see http://opensource.org/licenses/BSD-2-Clause for details
;;;;

;;;
;;; Game actions
;;;
(defun describe-location (location scenes)
  (cadr (assoc location scenes)))

(defun describe-path (path)
  `(there is a ,(caddr path) going ,(cadr path) from here.))

(defun describe-paths (location paths) 
  (apply #'append (mapcar #'describe-path (cdr (assoc location paths)))))

(defun objects-at (location objects object-locations)
  (labels ((at-location-p (obj)
			  (eq (cadr (assoc obj object-locations)) location)))
	   (remove-if-not #'at-location-p objects)))

(defun object-placement (obj obj-locs)
  (let ((thing (car (assoc obj obj-locs)))
	(surface (caddr (assoc obj obj-locs)))
	(place (intern 
		 (concatenate 'string (symbol-name (cadr (assoc obj obj-locs)))
			      "."))))
    `(you see a ,thing on ,surface of ,place)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (obj) (object-placement obj object-locations)))
    (apply #'append 
	   (mapcar #'describe-obj 
		   (objects-at location objects object-locations)))))

;;;
;;; Game Player Vocabulary
;;;

;; this is the list of permitted commands. At some point 
;; I should adopt a predictive naming convension and calculate the valid names in
;; the list with a macro or something.
(defparameter *allowed-commands* '(look walk crawl pickup drop inventory save player inspector))

(defun inspector ()
  (progn
    (princ "location: ")
    (princ *location*)
    (fresh-line)
    (princ "object locations: ")
    (princ *object-locations*)
    (fresh-line)
    (fresh-line)))

;;
;; look - explain the scene, paths and objects around the player
;;
(defun look ()
  "Returns three lists scene, paths away and objects available"
  (append (describe-location *location* *scenes*)
	  (describe-paths *location* *paths*)
	  (describe-objects *location* *objects* *object-locations*)))

;;
;; walk - move from one scene to the next.
;;
(defun walk (direction)
  "Args: direction is based on the paths available
  Returns new location description or error list"
  (let ((next (find direction
		    (cdr (assoc *location* *paths*))
		    :key #'cadr)))
    (if next
      (progn
	(setf *location* (car next))
	(look))
      '(you cannot go that way.))))

;;
;; crawl move inside a scene, location doens't change
;; e.g. crawl under couch, crawl into cabinet
;;
(defun crawl (direction)
  "Args: direction is based on the paths available without changing scene
  Returns new location description or error list"
  (walk direction))

;;
;; pickup an object and put in your inventory
;;
(defun pickup (object)
  "Args: object you wish to pickup
  Returns: list of whether the pickup was successful or not"
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t `(you cannot pickup ,object))))

;;
;; drop an object at the current location
;;
(defun drop (object)
  "Args: object to be dropped at current location
  Returns: message of drop success"
  (print "DEBUG drop not implemented"))

;;
;; inventory the player current possesses.
;;
(defun inventory ()
  "Returns the current list of items in the inventory"
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


;;;
;;; game-*, tweak-text functions are based directly on Land of Lisp examples.
;;; these will get replaced last since they are usefually generic.
;;;

;;
;; tweak-text mutates a symbol list into Human friendly format
;; e.g. capitalizes beginning of sentences, adjusts cases.
;;
(defun tweak-text (lst caps lit)
  "Args: lst, caps, lit
  Returns: the list as Human readable text."
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit))) 
	    ((or caps lit) (cons (char-upcase item)
				 (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;
;; prints the lists as Human friendly output using tweak-text
;;
(defun game-print (lst)
  "Args: list to print
  Returns: a fresh line"
  (fresh-line)
  (princ (coerce (tweak-text (coerce (string-trim "()"
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

;;
;; evaluates an S-Expression if valid command
;;
(defun game-eval (sexp)
  "Args: sexp - the S-Expression to convert
  Returns: the results of evaluating the S-Express or error message"
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

;;
;; read a command and turn it into an S-Expression
;;
(defun game-read ()
  "Returns: an S-expression of input"
  (let ((cmd (read-from-string
	       (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
		     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;
;; Game repl
;;
(defun game-repl () 
  "Side-effect: manages the game interaction"
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (cond ((not cmd) (game-print *allowed-commands*))
	    ((eq (car cmd) 'player) (princ (game-eval cmd)))
            ((eq (car cmd) 'save) (princ (game-eval cmd)))
	    (t (game-print (game-eval cmd))))
      (game-repl))))

;;;
;;; Game State - these functions are new and not included in Land of Lisp
;;;

;;
;; save the player's game state.
;;
(defun save (&optional (player-name "anonymous"))
  "Args: player-name as string
  Return true what was saved if successful save"
  (progn
    (setf *player-data* (list *location* *object-locations*))
    (with-open-file (out player-name
			 :direction :output
			 :if-exists :supersede)
      (with-standard-io-syntax 
	(print *player-data* out)))
    (inspector)
    (fresh-line)
    (game-print (look))))



;;
;; load a player's game state.
;;
(defun player (&optional (player-name "anonymous"))
  "Args:  player-name as string
  Side-effect: updates *location* and *object-locations*
  Returns: what was read"
  (progn
    (with-open-file (in player-name) 
      (with-standard-io-syntax 
	(setf *player-data*  (read in))))
    (setf *location* (car *player-data*))
    (setf *object-location* (cdr *player-data*))
    (inspector)
    (fresh-line)
    (game-print (look))))
 	

;;
;; Play the game
;;
(defun start-engine (&optional (game-data "wizards-cottage.lisp"))
  (progn 
    (if (load game-data)
      (progn
	(fresh-line)
	;TODO: load player state
	(fresh-line)
	(game-print (look))
	(fresh-line)
	(game-print (cons 'commands- *allowed-commands*))
	(fresh-line)
	(game-repl))
      (format t "Cannot load ~S" game-data))))

