;;;; holberg-app-suite/www/chord-generator.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package #:holberg-app-suite)

;;; move these to a separate file for use with resonance, chords, more

(defun chord-generator ()
  (with-page (:title "Chord Generator")
    (:header
     (:h1 "Chord Generator"))
    (:section
     (:p "A tool for generating chords on a variety of stringed instruments. Currently limited to first position chords."))
    (:section
     (:h2 "Select your instrument, chord root, and chord quality:")
     (spinneret:with-html
         (:form :action "/generate-chords" :id "generate-chords"
            (:select :name "instrument-option" :id "generate-chords"
              (loop :for i :in (mapcar #'first holberg-app-suite::*instrument-options*)
                    :do (:option :value i (format nil "~a" i))))
            (:select :name "root-option" :id "generate-chords"
              (loop :for i :in  holberg-app-suite::*root-options*
                    :do (:option :value (second i) (format nil "~a" (first i)))))
            (:select :name "quality-option" :id "generate-chords"
              (loop :for i :in holberg-app-suite::*chord-quality-options*
                    :do (:option :value i (format nil "~a" i))))
            (:input :type "submit" :value "Generate Chords" :class "button"))))))

(push (hunchentoot::create-prefix-dispatcher "/chord-generator.html" #'chord-generator)
      hunchentoot::*dispatch-table*)

(hunchentoot::define-easy-handler (generate-chords :uri "/generate-chords")
    (instrument-option root-option quality-option)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((chords (otakar::print-chords
                 (otakar::full-chords
                 (eval (second (assoc instrument-option *instrument-options* :test #'string-equal)))
                 (holberg::make-chord (parse-integer root-option) quality-option)))))
  (with-page (:title "Chord Generator")
    (:header
     (:h1 "Chord Generator")
     (spinneret:with-html
       (:h3 "All first-position voicings of"
         (format nil "~a ~a" (holberg::number-string (parse-integer root-option)) quality-option)
         (format nil "on the ~a" instrument-option))))
    (spinneret:with-html
      (:section
       (loop :for c :in chords
             :do (:p c)))))))
