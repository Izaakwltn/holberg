;;;; seria/web-matrix.lisp
;;;;
;;;;

(defpackage tone-matrix-generator
  (:documentation "Tone Matrix Web App")
  (:use #:cl #:seria #:cl-bootstrap #:cl-who #:spinneret #:hunchentoot))

(in-package #:tone-matrix-generator)


(defvar *matrix-server*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 4242
                 :document-root (asdf:system-relative-pathname "holberg" "seria/www/")))
                 
(defun launch ()
  (hunchentoot:start *server*))


(setf (cl-bootstrap::html-mode) :html5)

;(defmacro with-page ((&key title) &body body)
 ; `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
  ;   (:doctype)
   ;  (:html :lang "en"
    ;  (:head
     ;  (:meta :charset "utf-8")
      ; (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       ;(:link :type "text/css"
        ;      :rel "stylesheet"
	 ;     :href ,cl-bootstrap:*bootstrap-css-url*)
      ; (:title ,title)
       ;(:script :src ,cl-bootstrap:*jquery-url*)
   ;    (:script :src ,cl-bootstrap:*bootstrap-js-url*)))
    ; (:body
     ; (cl-bootstrap:bs-navbar (:brand "Tone Matrix Generator")
  ;    (cl-bootstrap:bs-container ()
   ;     (cl-bootstrap:bs-row
    ;      (cl-bootstrap:bs-col-md ()
     ;   ,@body)
   ;   (:footer "Bartleby the Scheduler - Copyright (c) 2021-2022 "
;	       (:a :href "https://www.github.com/izaakwltn/bartleby"
;		   "Written using Common Lisp"))))))))
