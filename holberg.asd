;;;; holberg.asd
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(asdf:defsystem "holberg"
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A suite for music analysis and generation"
  :depends-on (#:alexandria)
  :serial t
  :components ((:module "src"
                :serial t
                        :components ((:file "pitch-class")
	                             (:file "pitch-class-set")
                                     ;(:file "print-systems")
	                             (:file "pitch")
	                             (:file "collection")
                                     (:file "key")
	                             (:file "scale")
                                     (:file "chord")
                                     (:file "progression")
                                     (:file "romans")
                                     (:file "event")
	                             (:file "tuning")))))

(asdf:defsystem "holberg/seria"
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Tools for serial tone row manipulation"
  :depends-on ("holberg")
  :serial t
  :components ((:module "seria"
			:serial t
			:components ((:file "row")
				     (:file "matrix")))))

(asdf:defsystem "holberg/oatakar"
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Tools for stringed instrument analysis."
  :depends-on ("holberg")
  :serial t
  :components ((:module "otakar"
			:serial t
			:components ((:file "instrument")))))

#+ig(asdf:defsystem "holberg/app"
      :version "0.0.1"
      :author "Izaak Walton <izaakw@protonmail.com"
      :license "GNU General Purpose License"
      :description "Webapp for Holberg music tools"
      :depends-on ("holberg" "otakar" "seria")
      :serial t
      :build-operation program-op
      :build-pathname "launch-suite"
      :entry-point "holberg"
      )
               
