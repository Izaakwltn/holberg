;;;; holberg-app-suite/www/matrix-generator.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:holberg-app-suite)


(defun matrix-generator ()
  (with-page (:title "Serial Matrix Generator")
    (:header
     (:h1 "Tone Matrix Generator")
     (:h2 "A tool for making twelve tone serial matrices from a given tone row"))
    (:section
     (:h2 "Input your tone row below: ")
     (:form :action "/generate-matrix" :id "generate-matrix"
            (spinneret:with-html
              (:select :name "format-option" :id "generate-matrix"
              (loop :for i :in (mapcar #'first holberg-app-suite::*format-options*)
                    :do (:option :value i (format nil "~a" i)))))
            (:input :type "text" :id "generate-matrix" :name "tone-row")
            (:button :type "submit" :class "btn btn-default" "Generate Matrix")))
    (:section
     (:h2 "Or try a random row:")
     (:form :action "/random-matrix" :id "random-matrix"
            (spinneret:with-html
              (:select :name "format-option" :id "random-matrix"
                (loop :for i :in (mapcar #'first holberg-app-suite::*format-options*)
                      :do (:option :value i (format nil "~a" i))))
            (:button :type "submit" :class "btn btn-default" "Generate Random Matrix"))))))

(push (hunchentoot::create-prefix-dispatcher "/matrix-generator.html" #'matrix-generator)
      hunchentoot::*dispatch-table*)

(defun parse-tone-row (tone-row-string) ;"0 1 2 3" -> (0 1 2 3)
  (mapcar #'parse-integer
          (loop :with parsed := nil
                :with current := ""

                :for i :from 1 :to (length tone-row-string)
                :do (let ((x (subseq tone-row-string (1- i) i)))
                      (cond ((and (string-equal x " ")
                                  (> (length current) 0))
                             (progn (setq parsed (cons current parsed))
                                    (setq current "")))
                            ((string-equal x " ") nil)
                            (t (setq current (format nil "~a~a" current x)))))
                :finally (return (reverse (if (string-equal current "")
                                              parsed
                                              (cons current parsed)))))))

(hunchentoot::define-easy-handler (generate-matrix :uri "/generate-matrix")
    (tone-row format-option)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((matrix
          (funcall (eval (second
                    (assoc format-option *format-options* :test #'string-equal)))
                   (parse-tone-row tone-row))))
  (with-page (:title "Tone Row Matrix")
    (:header
     (:h1 "Your Twelve Tone Matrix"))
    (:section
     (:p (format nil "Prime row: ~a" tone-row)))
    (:section
      ;(spinneret:with-html
         (:table :border 2 :width "100%" :padding-bottom "100px" ;:cellspacing "5"
                 (:tbody
        (spinneret:with-html (loop :for row :in matrix
              :do (:tr (loop :for pc :in row
                             :do (:td (format nil " ~a " pc)))))))))
    (:section
     (:h2 "Try another tone row below: ")
     (:form :action "/generate-matrix" :id "generate-matrix"
            (spinneret:with-html
              (:select :name "format-option" :id "generate-matrix"
              (loop :for i :in (mapcar #'first holberg-app-suite::*format-options*)
                    :do (:option :value i (format nil "~a" i)))))
            (:input :type "text" :id "generate-matrix" :name "tone-row")
            (:button :type "submit" :class "btn btn-default" "Generate Matrix")))
    (:section
     (:h2 "Or try a random row:")
     (:form :action "/random-matrix" :id "random-matrix"
            (spinneret:with-html
              (:select :name "format-option" :id "random-matrix"
                (loop :for i :in (mapcar #'first holberg-app-suite::*format-options*)
                      :do (:option :value i (format nil "~a" i))))
            (:button :type "submit" :class "btn btn-default" "Generate Random Matrix")))))))


    ; (:p "yes")))));(if matrix "yes" "no")))))))

(hunchentoot::define-easy-handler (random-matrix :uri "/random-matrix")
    (format-option)
  (setf (hunchentoot:content-type*) "text/html")
  (let* ((tone-row (seria::random-row))
         (matrix
          (funcall (eval (second
                    (assoc format-option *format-options* :test #'string-equal)))
                   tone-row)))
  (with-page (:title "Tone Row Matrix")
    (:header
     (:h1 "Your Twelve Tone Matrix"))
    (:section
     (:p (format nil "Prime row: ~a" tone-row)))
    (:section
      ;(spinneret:with-html
         (:table :border 2 :width "100%" :style "font-size=30px" ;:cellspacing "5"
                 (:tbody
        (spinneret:with-html (loop :for row :in matrix
              :do (:tr (loop :for pc :in row
                             :do (:td (format nil " ~a " pc)))))))))
    (:section
     (:h2 "Try another tone row below:")
     (:form :action "/generate-matrix" :id "generate-matrix"
            (spinneret:with-html
              (:select :name "format-option" :id "generate-matrix"
              (loop :for i :in (mapcar #'first holberg-app-suite::*format-options*)
                    :do (:option :value i (format nil "~a" i)))))
            (:input :type "text" :id "generate-matrix" :name "tone-row")
            (:button :type "submit" :class "btn btn-default" "Generate Matrix")))
    (:section
     (:h2 "Or try a random row:")
     (:form :action "/random-matrix" :id "random-matrix"
            (spinneret:with-html
              (:select :name "format-option" :id "random-matrix"
                (loop :for i :in (mapcar #'first holberg-app-suite::*format-options*)
                      :do (:option :value i (format nil "~a" i))))
            (:button :type "submit" :class "btn btn-default" "Generate Random Matrix")))))))
