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
                                     ;(:file "progressions")
                                     ;(:file "romans")
                                     ;(:file "rhythm")
                                     ;(:file "notes")
                                     ;(:file "measures")
	                             ;(:file "freqs")
                                     ;(:file "overtones")                       
            ;                         (:file "tunings")
				     ))))

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
               
