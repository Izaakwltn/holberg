;;;; holberg-app-suite/main.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package #:holberg-app-suite)

(defun main ()
  "Executable entrypoint"
  (handler-case
      (progn
        (launch)
        (bt:join-thread
         (find-if (lambda (th)
                    (search "hunchentoot" (bt:thread-name th)))
                  (bt:all-threads))))
    (sb-sys:interactive-interrupt () (progn
                                       (format *error-output* "Error ~&")
                                       (uiop:quit)))
    (error (c) (format *error-output* "~&An error occured: ~A~&" c))))

