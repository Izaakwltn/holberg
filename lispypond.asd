;;;; lispypond.asd
;;;;
;;;; Copyright Izaak Walton (C) 2023

(asdf:defsystem #:lispypond
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Lilypond parsing, analysis, and generation library."
  :depends-on (#:holberg #:alexa)
  :serial t
  :components ((:module "lispypond"
                :serial t
                :components ((:file "package")
                             (:file "lexing")))))
