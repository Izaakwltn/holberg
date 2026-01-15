;;;; otakar/position.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023 - 2026

(defpackage #:otakar/position
  (:use #:cl)
  (:local-nicknames
   (#:str #:otakar.string)))

(in-package #:otakar/position)

;;; position is probably string agnostic

;;; might be useful to define this in physical measurements of the string on the instrument, against your hand frame

;;; going up the fingerboard, your hand can potentially cover more frets

;;; instrument will be defined with 
(defstruct hand-frame
  (reach ))
