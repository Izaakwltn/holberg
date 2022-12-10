;;;; holberg.asd
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(asdf:defsystem #:holberg
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A suite for music analysis and generation"
  :depends-on (#:alexandria #:alexa)
  :serial t
  :components ((:module "src"
                :serial t
                        :components ((:file "package")
	                             (:file "pitch-classes")
	                             (:file "pitch-class-sets")
                                     (:file "print-systems")
	                             (:file "pitches")
	                             (:file "collections")
                                     (:file "keys")
	                             (:file "scales")
                                     (:file "chords")
                                     (:file "progressions")
                                     (:file "romans")
                                     (:file "rhythm")
                                     (:file "notes")
                                     (:file "measures")
	                             (:file "freqs")
                                     (:file "overtones")                       
                                     (:file "tunings")))))
               
