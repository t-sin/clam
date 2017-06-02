(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:export :clam-loop))
(in-package :clam/main)

(defun clam-loop ()
  (format t "Common Lisp is Actually Marvelous!~%"))
