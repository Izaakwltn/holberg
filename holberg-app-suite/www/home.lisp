;;;; holberg-app-suite/www/home.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :holberg-app-suite)

(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (with-page (:title "Home")
    (:h1 "Holberg Suite toolkit")
    (:hr)))
