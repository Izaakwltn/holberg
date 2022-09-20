;;;; pitch-class-sets.lisp
;;;;
;;;;

(in-package :holberg)


;;; ------------------------------------------------------------------------
;;; Pitch Class Sets

(defun pc-set-p (ls)
  "Determines whether the list constitutes a pitch class set."
  (cond ((null ls) t)
	((not (typep (first ls) 'pitch-class))
	 nil)
	((member (first ls) (rest ls))
	 nil)
	(t (pc-set-p (rest ls)))))

(deftype pc-set ()
  "A pitch-class set"
  `(satisfies pc-set-p))

(defvar *n-element-sets* '((1 isochord)
			   (2 dyad)
			   (3 trichord)
			   (4 tetrachord)
			   (5 pentachord)
			   (6 hexachord)
			   (7 heptachord)
			   (8 octachord)
			   (9 nonachord)
			   (10 decachord)
			   (11 undecachord)
			   (12 dodecachord)))

(defun n-element-name (pc-set)
  "Returns the designated name for the length of pitch-class set"
  (check-type pc-set pc-set)
  (second (assoc (length pc-set)
		 *n-element-sets*)))

;;; Ordering Pitch-class sets

(defun ascending (pc-set)
  (check-type pc-set pc-set)
  (sort (copy-list pc-set) #'<))

(defun descending (pc-set)
  (check-type pc-set pc-set)
  (sort (copy-list pc-set) #'>))

(defun set-transpose (pc-set interval)
  "Transposes a pitch class set by a given interval"
  (check-type pc-set pc-set)
  (check-type interval integer)
  (mapcar #'(lambda (pc)
	      (pc-transpose pc interval))
	  pc-set))

;;; Pitch Class Set Permutation

(defun permutate (pc-set)
  "Shifts a pitch class set over by one"
  (check-type pc-set pc-set)
  (append (cdr pc-set) (list (car pc-set))))

;;; finding Normal Order/normal form

(defun first-last-interval (pc-set)
  "Finds the interval between the first PC and last PC in a PC set"
  (check-type pc-set pc-set)
  (pc-interval (first pc-set)
	       (car (last pc-set))))

(defun more-normal-p (pcs-1 pcs-2)
  "Finds the more normalized of two pc-sets"
  (check-type pcs-1 pc-set)
  (check-type pcs-2 pc-set)
  (cond ((equal (length pcs-1) 1)
	 t)
	((< (first-last-interval pcs-1)
	    (first-last-interval pcs-2))
	 t)
	((> (first-last-interval pcs-1)
	    (first-last-interval pcs-2))
	 nil)
	((equal (first-last-interval pcs-1)
		(first-last-interval pcs-2))
	 (more-normal-p (remove (car (last pcs-1))
				pcs-1)
			(remove (car (last pcs-2))
				pcs-2)))))

(defun normal-order (pc-set)
  "Returns the normal order for a pc-set (organized by smallest intervals)"
  (check-type pc-set pc-set)
  (loop :with permutated := (ascending pc-set)
	:with normalest  := permutated
	
	:for i :from 1 :to (length permutated)
	:if (more-normal-p permutated normalest)
	  :do (setq normalest permutated)
	:do (setq permutated (permutate permutated))
	:finally (return normalest)))

(defun normal-form (pc-set)
  "Returns the normal form for a given pitch-class set"
  (check-type pc-set pc-set)
  (let ((no (normal-order pc-set)))
    (set-transpose no (- (first no)))))

;;; Pitch Class Set Symmetry

(defun set-complement (pc-set)
  (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) pc-set))
;(defun t-related-p (pcs1 pcs2)
					; "Determines whether two Pitch Class Sets are related transpositionally."
 ; (check-type pcs1 pc-set)
  ;(check-type pcs2 pc-set)
 ; (equal (set-transpose pcs1 (- (first pcs1)
   
(defun t-symmetry (pc-set)
  "Returns the degree of transpositional symmetry, 
   the number of transpositions which return the same unordered pc-set"
  (check-type pc-set pc-set)
  (loop :with count := 0
	:for i :from 0 :to 11
	:if (equal (ascending pc-set)
		   (ascending (set-transpose pc-set i)))
	  :do (setq count (1+ count))
	:finally (return count)))


;;; Pitch Class Pitch definition

(defclass pitch-class-set ()
  ((pc-set      :initarg :pc-set
	        :accessor pc-set)
   (n-elem-name :initarg :n-elem-name
		:accessor n-elem-name)
   (normalized  :initarg :normalized
		:accessor normalized)))

(defmethod print-object ((obj pitch-class-set) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pc-set pc-set)
		     (n-elem-name n-elem-name)
		     (normalized normalized))
	obj
      (format stream "~a {~{ ~a~}}, Normal Form: {~{ ~a~}}" n-elem-name pc-set normalized)))) ; (find-quality pc-set)))))

(defun make-pitch-class-set (pc-set)
  (check-type pc-set pc-set)
  (make-instance 'pitch-class-set :pc-set pc-set
		                  :n-elem-name (n-element-name pc-set)
				  :normalized (normal-form pc-set)))
