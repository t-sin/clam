(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:export :clam-loop))
(in-package :clam/main)

(defun clam-read ()
  )

(defun clam-eval (args)
  )

(defun show-prompt ()
  (format t "clamshell $ "))

(defun clam-loop ()
  (loop
     :for status := (print (clam-eval (clam-read)))
     :initially (show-prompt)
     :while status
     :do (show-prompt)))
