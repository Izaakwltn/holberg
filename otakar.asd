;;;; otakar.asd
;;;;
;;;;

(asdf:defsystem #:otakar
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A library for stringed instrument composition and analysis."
  :depends-on (#:alexandria #:holberg)
  :serial t
  :components ((:module "otakar"
                :serial t
                :components ((:file "package")
                             (:file "instruments")
	                     (:file "chords")
	                     (:file "chord-generator"))))) ; probably change to chord-generation
