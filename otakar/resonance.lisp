;;;; otakar/resonance.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :otakar)

;;; Sympathetic Resonance Calculations

(declaim (ftype (function (freq freqs) list) collect-symps))
(defun collect-symps (freq string-freqs)
  "Collects all sympathetically resonant frequencies between a freq and a string set"
  (cond ((null string-freqs) nil)
        (t (append (holberg::compare-overtones freq (first string-freqs))
                   (collect-symps freq (rest string-freqs))))))

(defmethod sympathetics ((instrument instrument) freq)
  "Collects all sympathetically resonant frequencies when a freq is played on the instrument"
  (check-type freq freq)
  (collect-symps freq (strings instrument)))

(defmethod symp-rating ((instrument instrument) freq)
  "Returns the total number of sympathetic vibrations when a frequency is played on an instrument."
  (check-type freq freq)
  (length (collect-symps freq (strings instrument))))

(declaim (ftype (function (freq freqs) list) symps-by-string))
(defun symps-by-string (freq string-freqs)
  "Collects a list of sympathetic vibrations on each string"
  (cond ((null string-freqs) nil)
        (t (cons (holberg::compare-overtones freq (first string-freqs))
                 (symps-by-string freq (rest string-freqs))))))

(defmethod instrument-symps ((instrument instrument) freq)
  (symps-by-string freq (strings instrument)))


  
                            
