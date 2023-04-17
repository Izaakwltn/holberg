;;;; holberg-app-suite/www/options.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:holberg-app-suite)

;;; Options for a variety of webapps/tools

(defvar *instrument-options* (mapcar #'(lambda (x)
                                         (list (otakar::name x)
                                               x))
                                     (reverse otakar::*instruments*)))

(defvar *root-options* (loop :for i :from 0 :to 11
                             :collect (list (holberg::number-string i) i)))

(defvar *tonic-options* (loop :for i :from 0 :to 11
                              :collect (list (holberg::number-string i) i)))

(defvar *chord-quality-options* (mapcar #'first holberg::*chord-qualities*))

(defvar *key-quality-options* (mapcar #'first holberg::*key-list*))


;;;for tone matrix generator

(defvar *format-options* '(("Numeric/Pitch Classes" #'seria::make-matrix)
                           ("Standard (0-9te)" #'seria::standard-matrix)
                           ("Note Names" #'seria::note-matrix)))
