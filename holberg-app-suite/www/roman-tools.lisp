;;;; holberg-app-suite/www/roman-tools.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:holberg-app-suite)

;;;

(defun roman-tools ()
  (with-page (:title "Roman Numeral Analysis")
    (:header
     (:h1 "Tools for Roman Numeral Analysis"))
    (:section
     (:h3 "Converting Chords to Roman Numerals")
     (:p "Enter the tonic of your key and your requested chords in this form:"
         "\"g g-major c-major d-major e-minor\""
         "Where the first g is the tonic of your key, and the subsequent values are root-quality pairs.")
     (:form :action "/chords-to-romans" :id "chords-to-romans"
            (:input :type "text" :id "chords-to-romans" :name "chord-string")
            (:button :type "submit" :class "btn btn-default" "Convert")))
    (:section
     (:h3 "Converting Roman Numerals to Chords")
     (:p "Enter the tonic of your key and your requested chords in this form:"
         "\"g I IV iv V vi\""
         "Where the 'g' is the tonic of your key, followed by roman numerals.")
     (:form :action "/romans-to-chords" :id "romans-to-chords"
            (:input :type "text" :id "romans-to-chords" :name "roman-string")
            (:button :type "submit" :class "btn btn-default" "Convert")))))

(push (hunchentoot::create-prefix-dispatcher "/roman-tools.html" #'roman-tools)
      hunchentoot::*dispatch-table*)

(hunchentoot::define-easy-handler (chords-to-romans :uri "/chords-to-romans")
    (chord-string)
  (setf (hunchentoot:content-type*) "text/html")
  (with-page (:title "Tools for Roman Numeral Analysis")
    (:header
     (:h1 "Chords to Roman Numeral conversion"))
    (:section
     (spinneret:with-html
      (:h3 "Your input:")
      (:h4 chord-string)
      (:h3  "Output:")
      (:h4 (holberg::print-romans
            (holberg::process-chord-input chord-string)))))
    (:br)
    (:h2 "Or try another:")
    (:section
     (:h3 "Converting Chords to Roman Numerals")
     (:p "Enter the tonic of your key and your requested chords in this form:"
         "\"g g-major c-major d-major e-minor\""
         "Where the first g is the tonic of your key, and the subsequent values are root-quality pairs.")
     (:form :action "/chords-to-romans" :id "chords-to-romans"
            (:input :type "text" :id "chords-to-romans" :name "chord-string")
            (:button :type "submit" :class "btn btn-default" "Convert")))
    (:section
     (:h3 "Converting Roman Numerals to Chords")
     (:p "Enter the tonic of your key and your requested chords in this form:"
         "\"g I IV iv V vi\""
         "Where the 'g' is the tonic of your key, followed by roman numerals.")
     (:form :action "/romans-to-chords" :id "romans-to-chords"
            (:input :type "text" :id "romans-to-chords" :name "roman-string")
            (:button :type "submit" :class "btn btn-default" "Convert")))))

(hunchentoot::define-easy-handler (romans-to-chords :uri "/romans-to-chords")
    (roman-string)
  (setf (hunchentoot:content-type*) "text/html")
  (with-page (:title "Tools for Roman Numeral Analysis")
    (:header
     (:h1 "Chords to Roman Numeral conversion"))
    (:section
     (spinneret:with-html
      (:h3 "Your input:")
      (:h4 roman-string)
      (:h3  "Output:")
       (:h4 (holberg::process-roman-input roman-string))))
    (:br)
    (:h2 "Or try another:")
    (:section
     (:h3 "Converting Chords to Roman Numerals")
     (:p "Enter the tonic of your key and your requested chords in this form:"
         "\"g g-major c-major d-major e-minor\""
         "Where the first g is the tonic of your key, and the subsequent values are root-quality pairs.")
     (:form :action "/chords-to-romans" :id "chords-to-romans"
            (:input :type "text" :id "chords-to-romans" :name "chord-string")
            (:button :type "submit" :class "btn btn-default" "Convert")))
    (:section
     (:h3 "Converting Roman Numerals to Chords")
     (:p "Enter the tonic of your key and your requested chords in this form:"
         "\"g I IV iv V vi\""
         "Where the 'g' is the tonic of your key, followed by roman numerals.")
     (:form :action "/romans-to-chords" :id "romans-to-chords"
            (:input :type "text" :id "romans-to-chords" :name "roman-string")
            (:button :type "submit" :class "btn btn-default" "Convert")))))
