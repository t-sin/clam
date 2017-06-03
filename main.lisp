(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:export :clam-loop))
(in-package :clam/main)

(defun show-prompt ()
  (format *standard-output* "~&clamshell$ ")
  (finish-output *standard-output*))

(defvar *clam-delimiter* #(#\space #\tab))

(defun tokenize (line)
  (let ((tokens (make-array 1 :adjustable t :fill-pointer 0))
        (buffer  (make-string-output-stream)))
    (labels ((store-token (token)
               (when (plusp (length token))
                 (vector-push-extend token tokens)))
             (for-char (c)
               (if (position c *clam-delimiter*)
                   (progn
                     (store-token (get-output-stream-string buffer))
                     (setf buffer (make-string-output-stream)))
                   (write-char c buffer))))
      (loop
         :for c :across line
         :do (for-char c)
         :finally (store-token (get-output-stream-string buffer))
         (return-from tokenize tokens)))))

(defun clam-read ()
  (show-prompt)
  (tokenize (read-line)))

(defun clam-eval (args)
  args)

(defun clam-print (object)
  (progn
    (format *standard-output* "~s" object)
    (finish-output *standard-output*)
    object))

(defun clam-loop ()
  (loop
     :for status := (clam-print (clam-eval (clam-read)))
     :while status))
