;;;; holberg.asd
;;;;
;;;; Copyright (c) 2022-2025 Izaak Walton

(asdf:defsystem #:holberg
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "An intermediate representation of music data."
  :depends-on (#:alexandria)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "pitch-class")
			     (:file "pitch")
			     (:file "collection")
			     (:file "key")
			     (:file "scale")))))
			     ;(:file "chords")

;; (:file "progressions")
;; (:file "romans")
;; (:file "rhythm")
;; (:file "notes")
;; (:file "measures")
;; (:file "freqs")
;; (:file "overtones")                       
;; (:file "tunings")))))
               
