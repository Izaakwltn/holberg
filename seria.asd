;;;; seria.asd
;;;;
;;;; Copyright Izaak Walton (c) 2022

(asdf:defsystem #:seria
  :version "0.0.2"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A Module for building and analyzing 12-tone serial rows and matrices."
  :depends-on (#:holberg)
  :serial t
  :components ((:module "seria"
	       :serial t ; well of course it's serial...
	       :components ((:file "package")
			    (:file "rows")
			    (:file "row-functions")
			    (:file "matrices")
			    (:file "format")))))
