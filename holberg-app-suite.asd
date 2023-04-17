;;;; holberg-app-suite.asd
;;;;
;;;; Copyright Izaak Walton (c) 2022

(asdf:defsystem #:holberg-app-suite
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com"
  :license "GNU General Purpose License"
  :description "Webapps for Holberg music tools"
  :depends-on ("holberg" "otakar" "seria" "cl-who" "spinneret" "cl-bootstrap" "hunchentoot")
  :serial t
  :build-operation program-op
  :build-pathname "launch-suite"
  :entry-point "holberg-app-suite::main"
  :components ((:module "holberg-app-suite"
                :serial t
                :components ((:file "package")
                             (:file "server")
                             (:file "main")
                             (:file "page-template")
                             (:module "www"
                              :serial t
                              :components ((:file "home")
                                           (:file "options")
                                           (:file "resonance-input")
                                           (:file "chord-generator")
                                           (:file "roman-tools")
                                           (:file "matrix-generator")))))))
                             
