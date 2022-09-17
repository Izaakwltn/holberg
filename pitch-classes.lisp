;;;; pitch-classes.lisp
;;;;
;;;;

(in-package :holberg)

;;; Pitch Class Type and basic operations

(defun pitch-class-p (n)
  "Determines whether an integer is a qualifying pitch class"
  (check-type n integer)
  (and (>= n 0)
       (<= n 11)))

(deftype pitch-class ()
  `(satisfies pitch-class-p))

(defun pc-incr (pc)
  "Increments a pitch class"
  (check-type pc pitch-class)
  (mod (1+ pc) 12))

(defun pc-decr (pc)
  "Decrements a pitch class"
  (check-type pc pitch-class)
  (mod (1- pc) 12))

(defun pc-transpose (pc interval)
  "Transposes a pitch class up or down by a given signed integer"
  (check-type pc pitch-class)
  (check-type interval integer)
  (cond ((zerop interval) pc)
	((< interval 0)
	 (pc-transpose (pc-decr pc) (1+ interval)))
	((> interval 0)
	 (pc-transpose (pc-incr pc) (1- interval)))))

;;; Finding intervals between pitch classes

(defun pc-interval-backend (pclow pchigh interval)
  "Backend for pc-interval"
  (cond ((equal pclow pchigh) interval)
	(t (pc-interval-backend (pc-incr pclow) pchigh (1+ interval)))))

(defun pc-interval (pclow pchigh)
  "Finds the interval in between two pitch-classes"
  (check-type pclow pitch-class)
  (check-type pchigh pitch-class)
  (pc-interval-backend pclow pchigh 0))

;;; ------------------------------------------------------------------------
;;; Pitch Class Sets

;(defun pc-set-p (ls)
 ; "Determines whether the list constitutes a pitch class set."
  ;(cond ((null ls) t)
;	((not (typep (first ls) 'pitch-class))
;	 nil)
;	((member (first ls) (rest ls))
;	 nil)
;	(t (pc-set-p (rest ls)))))

;(deftype pc-set ()
 ; "A pitch-class set"
  ;`(satisfies pc-set-p))

;(defvar *n-element-sets* '((1 isochord)
;			   (2 dyad)
;			   (3 trichord)
;			   (4 tetrachord)
;			   (5 pentachord)
;			   (6 hexachord)
;			   (7 heptachord)
;			   (8 octachord)
;			   (9 nonachord)
;			   (10 decachord)
;			   (11 undecachord)
;			   (12 dodecachord)))

;(defun n-element-name (pc-set)
 ; (check-type pc-set pc-set)
  ;(second (assoc (length pc-set)
;		 *n-element-sets*)))

;;; Ordering Pitch-class sets

;(defgeneric ascending (object)
;  (:documentation "Sorts an object's list into an ascending order"))

;(defun ascending (pc-set)
 ; (check-type pc-set pc-set)
  ;(sort (copy-list pc-set) #'<))

;(defgeneric descending (object)
 ; (:documentation "Sorts an object's list into a descending order"))

;(defun descending (pc-set)
 ; (check-type pc-set pc-set)
  ;(sort (copy-list pc-set) #'>))

;;; Pitch Class Set Permutation
;(defun permutate (pc-set)
 ; (check-type pc-set pc-set)
  ;(append (cdr pc-set) (list (car pc-set))))

;;; finding Normal Order/normal form

;(defun first-last-interval (pc-set)
 ; "Finds the interval between the first PC and last PC in a PC set"
  ;(check-type pc-set pc-set)
;  (pc-interval (first pc-set)
;	       (car (last pc-set))))

;(defun more-normal-p (pcs-1 pcs-2)
;  "Finds the more normalized of two pc-sets"
 ; (check-type pcs-1 pc-set)
;  (check-type pcs-2 pc-set)
  ;(cond ((equal (length pcs-1) 1)
;	 t)
;	((< (first-last-interval pcs-1)
;	    (first-last-interval pcs-2));
;	 t)
;	((> (first-last-interval pcs-1)
;	    (first-last-interval pcs-2))
;	 nil)
;	((equal (first-last-interval pcs-1)
;		(first-last-interval pcs-2))
;	 (more-normal-p (remove (car (last pcs-1))
;				pcs-1)
;			(remove (car (last pcs-2))
;				pcs-2)))))

;(defun normal-order (pc-set)
 ; (check-type pc-set pc-set)
  ;(loop :with permutated := (ascending pc-set)
;	:with normalest  := permutated
;	
;	:for i :from 1 :to (length permutated)
;	:if (more-normal-p permutated normalest)
;	  :do (setq normalest permutated)
;	:do (setq permutated (permutate permutated))
;	:finally (return normalest)))
;
;(defun set-transpose (pc-set interval)
 ; "Transposes a pitch class set by a given interval"
  ;(check-type pc-set pc-set)
;  (check-type interval integer)
 ; (mapcar #'(lambda (pc)
;	      (pc-transpose pc interval))
;	  pc-set))

;(defun normal-form (pc-set)
;  "Returns the normal form for a given pitch-class set"
;  (check-type pc-set pc-set)
;  (let ((no (normal-order pc-set)))
;    (set-transpose no (- (first no)))))


