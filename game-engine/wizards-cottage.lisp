;;;
;;; Wizard's Cottage Game - is based on the simple example in Conrad Barski's book.
;;; A few extra locations are included that extend the three in the book.
;;;

;; The default  starting location of the game.
(defparameter *location* 'garden)

;;
;; Scenes are desctiptions of locations.
;;
(defparameter *scenes* '((living-room (you are in a living room. their is 
					   snoring coming from one of the doors in 
					   the room. There is a growl from 
					   under-the-couch and a click from the clock on 
					   the wall. there is a door on the north side
					   and one of the south side. there is a ladder
					   going upstairs.))
			 (under-the-couch (it is dimmer but you make out a small dog
					      wagging his tail. he looks sad and
					      affraid.))
			 (bedroom (on a futon you see a wizard snoring. there is a pile
				       of dirty laundry in the corner. there is a 
				       whiskey bottle on the floor.))
			 (garden (you are in a beautiful garden but some of the 
				      plants are very strange. There is a menencing buzz
				      from from under the bush. There are beautiful
				      hummingbirds feeding on the Mexican sage. there is a door 
				      to a cottage on east side.))
			 (attic (you are in an attic. it is dusty and with the
				     faint odor of musk. There is a cabnete locked
				     in the north corner and a window at the south end.
				     A mirror is tilted down along the east side of
				     the attic.))
			 (kitchen (you are in a very clean kitchen. There are knives
				       on the wall which look very sharp. The spice-safe
				       is locked with an intrecate looking
				       key whole. an oven in in the north corner and
				       a root-celler's trap door to in the south. a shelf
				       holds various baking
				       implements. there is a barrel marked flower
				       in the south east corner. A large table in
				       the middle of the room.  In the north east
				       corner there is a set of baskets. you can
				       see eggs and some fresh vegitables. there
				       is a stove in the west side with a large
				       soop pot simmering. there is an oven on the
				       east side.))))

;;
;; edges you can travel between scenes
;;
(defparameter *paths* '((living-room (garden west door)
					   (attic upstairs ladder)
					   (couch crawl couch)
					   (kitchen south door)
					   (bedroom north door))
			      (garden (living-room east door))
			      (couch (living-room crawl out))
			      (kitchen (living-room north door))
			      (bedroom (living-room south door))))



;;
;; objects in world
;;
(defparameter *objects* '(whiskey bucket frog chain flour matches eggs veggies puppy milk
				  sugar spice-cabnet-key mixing-bowl cookie-sheet 
				  spoon pot soup cookies))

;;
;; object locations
;;
(defparameter *object-locations* '((whiskey bedroom floor)
				   (bucket living-room floor)
				   (puppy couch floor)
				   (chain garden ground)
				   (frog garden ground)
				   (flour barrel top)
				   (matches spice-cabnet shelf)
				   (eggs basket inside)
				   (veggies basket inside)
				   (milk root-cellar floor)
				   (sugar spice-cabinet shelf)
				   (spice-cabinet-key table top)
				   (mixing-bowl cubbard shelf)
				   (cookie-sheet cubbard shelf)
				   (spoon cubbard drawer)
				   (soup-pot stove top)
				   (pie oven inside)))




