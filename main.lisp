(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:export :clam-shell))
(in-package :clam/main)

(defun show-prompt ()
  (format *standard-output* "~&clamshell$ ")
  (finish-output *standard-output*))

(defvar *clam-delimiter* #(#\space #\tab))
(defvar *clam-quotes* #(#\' #\"))

(defun tokenize (line)
  (let ((tokens (make-array 1 :adjustable t :fill-pointer 0))
        (buffer  (make-string-output-stream))
        (quote-char nil))
    (labels ((store-token (token)
               (when (plusp (length token))
                 (vector-push-extend token tokens)))
             (for-char (c)
               (cond ((position c *clam-delimiter*)
                      (if quote-char
                          (write-char c buffer)
                          (progn
                            (store-token (get-output-stream-string buffer))
                            (setf buffer (make-string-output-stream)))))
                     ((and quote-char (char= c quote-char))
                      (setf quote-char nil)
                      (write-char c buffer))
                     ((position c *clam-quotes*)
                      (setf quote-char c)
                      (write-char c buffer))
                     (t (write-char c buffer)))))
      (loop
         :for c :across line
         :do (for-char c)
         :finally (store-token (get-output-stream-string buffer))
         (return-from tokenize tokens)))))

(defun clam-read ()
  (show-prompt)
  (tokenize (read-line)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *clam-built-in-commands* nil)
  (defmacro define-command (name &body body)
    (let ((%name (intern (string-downcase (symbol-name name)) :keyword)))
      `(let ((command (cons ',%name #'(lambda (&rest args)
                                        (declare (ignorable args))
                                        ,@body))))
         (pushnew command
                  *clam-built-in-commands*
                  :key #'car)))))

(defun get-command (name)
  (cdr (find name *clam-built-in-commands* :key #'car)))

(define-command exit nil)

(defvar +clam-do-nothing+ (gensym))
(define-command |:| +clam-do-nothing+)

(defun clam-eval (args)
  (let* ((args (coerce args 'list))
         (ret t)
         (built-in-command (get-command (intern (string-downcase (first args)) :keyword))))
    (if built-in-command
        (setf ret (apply built-in-command (rest args)))
        (format t "external: ~a ~s~%" (first args) (rest args)))
    ret))

(defun clam-print (object)
  (when object
    (unless (eq object +clam-do-nothing+)
      (format *standard-output* "~s" object)
      (finish-output *standard-output*))
    object))

(defun clam-shell ()
  (loop
     :for status := (clam-print (clam-eval (clam-read)))
     :while status))
