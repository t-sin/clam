(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:export :clam-loop))
(in-package :clam/main)

(defun show-prompt ()
  (format *standard-output* "~&clamshell$ ")
  (finish-output *standard-output*))

(defun clam-read ()
  (show-prompt)
  (read-line))

(defun clam-eval (args)
  args)

(defun clam-print (object)
  (progn
    (format *standard-output* "~a" object)
    (finish-output *standard-output*)
    object))

(defun clam-loop ()
  (loop
     :for status := (clam-print (clam-eval (clam-read)))
     :while status))
