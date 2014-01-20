;;;
;;; test-one.lisp - the first test of immersive-text-engine.
;;; It presents the data of Barski's Wizard's cottage and
;;; runs through the non-repl commands to see if everything works
;;; as expected.
;;;

;;
;; Setup the game data as described in Barski's Land of Lisp
;; chapter 5
;;
;; Some adjustments to globals have been made. 
;; + *nodes* -> *scenes*
;; + *edges* -> *paths*
;;
(print "Setting up test data")

(defparameter *scenes* '((living-room
			   (you are in the living-room.
				a wizard is snoring loudly on the couch.))
			 (garden (you are in a beautiful garden.
				      there is a well in front of you.))
			 (attic (you are in the attic. there is a giant welding
				     torch in the corner.))))

(defparameter *paths* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))
				     
(defparameter *objects* '(whiskey bucket frog chain pie))

(defparameter *object-locations* '((whiskey living-room floor)
				   (bucket living-room floor)
				   (frog garden ground)
				   (chain garden ground)
				   (pie cubbard shelf)))

(print "Re-loading game-engine")
(load "game-engine.lisp")
(print "Setting up tests methods")
(defparameter *errors-found* nil)

;;
;; Assert functions
;;
(defun assert-equal (expected provided &optional (test-label nil))
  (let ((expected-results expected)
	(provided-results provided))
      (if (not (equal expected-results provided-results))
	(progn
	  (if test-label (print test-label))
	  (setq *errors-found* t)
	  (print "expected:")
	  (print expected-results)
	  (print "provided:")
	  (print provided-results)))))


;;
;; Test code
;;
(print "Running some tests")

(print "Testing describe-location")
(assert-equal 
  '(you are in the living-room. a wizard is snoring loudly on the couch.)
  (describe-location 'living-room *scenes*))

(assert-equal
  '(you are in a beautiful garden. there is a well in front of you.)
  (describe-location 'garden *scenes*))

(print "Testing descibe-path")
(assert-equal
  '(THERE IS A DOOR GOING WEST FROM HERE.)
  (describe-path '(garden west door)))

(assert-equal
  '(THERE IS A LADDER GOING UPDATES FROM HERE.) 
  (describe-path '(attic updates ladder)))

(assert-equal
  '(THERE IS A DOOR GOING EAST FROM HERE.) 
  (describe-path '(living-room east door)))

(print "Testing describe-paths")
(assert-equal
  '(there is a door going west from here.
	  there is a ladder going upstairs from here.)
  (describe-paths 'living-room *paths*))

(print "Testing objects-at")
(assert-equal
  '(whiskey bucket)
  (objects-at 'living-room *objects* *object-locations*))

(assert-equal
  '(frog chain)
  (objects-at 'garden *objects* *object-locations*))

(print "Testing object-placement")
(assert-equal
  '(you see a whiskey on floor of living-room.)
  (object-placement 'whiskey *object-locations*))

(assert-equal
  '(you see a pie on shelf of cubbard.)
  (object-placement 'pie *object-locations*))

(print "Testing describe-objects")
(assert-equal
  '(you see a whiskey on floor of living-room. you see a bucket on floor of living-room.)
  (describe-objects 'living-room *objects* *object-locations*))

(print "Testing look")
(setq *location* 'living-room)
(assert-equal
  '(YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH. THERE IS
	 A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE. YOU
	  SEE A WHISKEY ON FLOOR OF LIVING-ROOM. YOU SEE A BUCKET ON FLOOR OF LIVING-ROOM.)
  (look))
(setq *location* 'garden)
(assert-equal
  '(YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU. THERE IS A
	DOOR GOING EAST FROM HERE. YOU SEE A FROG ON GROUND OF GARDEN. YOU SEE A CHAIN ON
	GROUND OF GARDEN.)
  (look))


(print "Testing walk")
(setq *location* 'living-room)
(assert-equal
  '(YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU. THERE IS A
        DOOR GOING EAST FROM HERE. YOU SEE A FROG ON GROUND OF GARDEN. YOU SEE A CHAIN ON
	 GROUND OF GARDEN.)
  (walk 'west))

(assert-equal
  'garden
  *location*)

(assert-equal
  '(YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH. THERE IS
	 A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE. YOU
	  SEE A WHISKEY ON FLOOR OF LIVING-ROOM. YOU SEE A BUCKET ON FLOOR OF LIVING-ROOM.)
  (walk 'east))

(assert-equal
  'living-room
  *location*)

(print "Testing pickup")
(setq *location* 'living-room)
(assert-equal
  '(you are now carrying the whiskey)
  (pickup 'whiskey))

(print "Testing inventory")
(assert-equal
  '(ITEMS- WHISKEY)
  (inventory))

(print "To test game-read type the following (game-read) then type a line of text, should return a list.")

(fresh-line)
(if *errors-found*
  (print "Failed!")
  (print "Success!"))
(fresh-line)

