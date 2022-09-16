;;;; sets.lisp
;;;;
;;;;

(in-package :holberg)

(defclass note-collection ()
  ((note-list :initarg :note-list
	      :accessor note-list)))

(defmethod print-object ((obj note-collection) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((note-list note-list))
	obj
      (format stream "~{~a~%~}" note-list))))

(defun same-note-p (note1 note2)
  (and (equal (note-num note1)
	      (note-num note2))
       (equal (octave   note1)
	      (octave   note2))))

(defun remove-duplicate-notes (note-list)
  (remove-duplicates note-list :test #'same-note-p))

;(defun note-collection-p (note-list)
 ; (loop :for i :in )
  
(defun make-collection (note-list)
  (make-instance 'note-collection :note-list (remove-duplicate-notes note-list)))

(defun higher-note-p (note1 note2)
  (cond ((> (octave note1) (octave note2))
	 t)
	((> (note-num note1) (note-num note2))
	 t)
	(t nil)))

(defun lower-note-p (note1 note2)
  (cond ((< (octave note1) (octave note2))
	 t)
	((< (note-num note1) (note-num note2))
	 t)
	(t nil)))

(defgeneric ascending (object)
  (:documentation "Sorts an object's list into an ascending order"))

(defmethod ascending ((note-collection note-collection))
  (make-collection (sort (note-list note-collection)
		  #'lower-note-p)))

(defgeneric descending (object)
  (:documentation "Sorts an object's list into a descending order"))

(defmethod descending ((note-collection note-collection))
  (make-collection (sort (note-list note-collection)
		  #'higher-note-p)))

;;; pitch class sets (probably move to a new file)

(defclass pc-set ()
  ((pc-list :initarg :pc-list
	    :accessor pc-list))  
  (:documentation "Pitch class set object"))

(defvar *n-element-sets* '((1 isochord) ; maybe find a better term, though I'm partial to it
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

(defmethod n-element-type ((pc-set pc-set))
  (second (assoc (length (pc-list pc-set))
		 *n-element-sets*)))
			    

(defmethod print-object ((obj pc-set) stream)
  (print-unreadable-object (obj stream :type t)
      (format stream "~a {~{ ~a~}}" (n-element-type obj) (pc-list obj))))

(defun pitch-class-p (n)
  (note-num-p n))

(deftype pitch-class ()
  `(satisfies pitch-class-p))

(defun make-pc-set (pc-list)
  (make-instance 'pc-set :pc-list pc-list))

(defmethod note-to-pc-set ((note-collection note-collection))
  (make-pc-set (remove-duplicates (mapcar #'note-num (note-list note-collection)))))

(defmethod ascending ((pc-set pc-set))
  (make-pc-set (sort (pc-list pc-set) #'<)))

(defun pc-incr (pc)
  "Increments a pitch class"
  (mod (1+ pc) 12))

(defun pc-decr (pc)
  "Decrements a pitch class"
  (mod (1- pc) 12))

(defun pc-transpose (pc interval)
  (cond ((zerop interval) pc)
	((< interval 0)
	 (pc-transpose (pc-decr pc) (1+ interval)))
	((> interval 0)
	 (pc-transpose (pc-incr pc) (1- interval)))))

(defun pc-interval-backend (pclow pchigh interval)
  (cond ((equal pclow pchigh) interval)
	(t (pc-interval-backend (pc-incr pclow) pchigh (1+ interval)))))

(defun pc-interval (pclow pchigh)
  (pc-interval-backend pclow pchigh 0))
;;; Finding normal order/normal form

(defun first-last-interval (pc-list)
  "Finds the interval between the first PC and last PC in a PC set"
  (pc-interval (first pc-list)
	       (car (last pc-list))))
  
(defun permutate (pc-list)
  (append (cdr pc-list) (list (car pc-list))))

;if (equal (first-last-interval pc-list)
;(defun normal-orderizer (pc-list)
 ; (loop :with normalized := (13)
;	:with perm-list  := pc-list
;	
;	:for i :from 1 :to (length pc-list)
;	:if (<  (first normalized) (first-last-interval pc-list))
;	  :do (progn (setq normalized))))

(defun more-normal-p (pcs-1 pcs-2)
  "Finds the more normalized of two pc-sets"
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
			
(defmethod normal-order ((pc-set pc-set))
  (loop :with permutated := (pc-list (ascending pc-set))
	:with normalest  := permutated
	
	:for i :from 1 :to (length permutated)
	:if (more-normal-p permutated normalest)
	  :do (setq normalest permutated)
	:do (setq permutated (permutate permutated))
	:finally (return (make-pc-set normalest))))

(defmethod transpose ((pc-set pc-set) interval)
  (make-pc-set (mapcar #'(lambda (pc)
			   (pc-transpose pc interval))
		       (pc-list pc-set))))

(defmethod normal-form ((pc-set pc-set))
  (let ((no (normal-order pc-set)))
    (transpose no (- (first (pc-list no))))))
