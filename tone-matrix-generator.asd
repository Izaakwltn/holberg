;;;; tone-matrix-generator.asd
;;;;
;;;;

(asdf:defsystem #:tone-matrix-generator
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A web-app for generating tone-row matrices."
  :depends-on (#:holberg #:hunchentoot #:cl-who #:spinneret #:cl-bootstrap)
  :serial t
  :components ((:module "seria"
	       :serial t ; well of course it's serial...
	       :components ((:module "www"
			      :serial t
			      :components ((:file "web-matrix")))))))
