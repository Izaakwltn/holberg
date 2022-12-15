;;;; holberg-app-suite/server.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package #:holberg-app-suite)

(defvar *server*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 4242
                 :document-root (asdf:system-relative-pathname "holberg" "holberg-app-suite/www/")))

(defun start ()
  "Starts the hunchentoot server"
  (hunchentoot::start *server*))

(defparameter +format-string+
  #+(or win32 mswindows window)
  "exporer ~S"
  #+(or macos darwin)
  "open ~S"
  #-(or win32 mswindows macos darwin windows)
  "xdg-open ~S")

(defun open-browser (url)
  (uiop:run-program (format nil +format-string+ url)))

(defun launch ()
  (start)
  (open-browser "http://127.0.0.1:4242"))


