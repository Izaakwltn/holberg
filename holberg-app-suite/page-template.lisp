;;;; holberg-app-suite/page-template.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :holberg-app-suite)

(setf (cl-bootstrap::html-mode) :html5)

(defmacro with-page ((&key title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:doctype)
     (:html :lang "en"
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:link :type "text/css"
              :rel "stylesheet"
	      :href ,cl-bootstrap:*bootstrap-css-url*)
       (:title ,title)
       (:script :src ,cl-bootstrap:*jquery-url*)
       (:script :src ,cl-bootstrap:*bootstrap-js-url*)))
     (:body
      (cl-bootstrap:bs-navbar (:brand "Holberg")
        (cl-bootstrap:bs-navbar-nav ()
          (cl-bootstrap:bs-nav-li (:href "/resonance.html") "Resonance Calculator")
          (cl-bootstrap:bs-nav-li (:href "/chord-generator.html") "Chord Generator")
          (cl-bootstrap:bs-nav-li (:href "/roman-tools.html") "Roman Numeral Tools")
          (cl-bootstrap:bs-nav-li (:href "/matrix-generator.html") "Tone Matrix Generator")))
      (cl-bootstrap:bs-container ()
        (cl-bootstrap:bs-row
          (cl-bootstrap:bs-col-md ()
            ,@body)
          (:br)
          (:footer 
                   "Holberg App Suite - Copyright (c) 2022-2023 "
              (:a :href "https://www.github.com/Izaakwltn/holberg"
                  "Written using Common Lisp")))))))
