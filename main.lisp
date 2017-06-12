(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:export :clam-shell))
(in-package :clam/main)

(defun show-prompt ()
  (format *standard-output* "~&clamshell $ ")
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

(defvar +clam-status-exit+ (gensym))
(define-command exit +clam-status-exit+)

(defvar +clam-status-do-nothing+ (gensym))
(define-command |:| +clam-status-do-nothing+)

(defvar +clam-help-string+
  "CLAMCHOWDEEEEERRRRRRRR!!!

This is a help message for clamshell.")
(define-command help
  (format t "~a~%" +clam-help-string+))

(define-command sframe
  (let ((w (read-from-string (first args)))
        (h (read-from-string (second args)))
        (s (third args)))
    (flet ((fills (s len)
             (let ((slis (list s)))
               (rplacd (last slis) slis)
               (format nil "~v{~a~}" (ceiling (/ len (length s))) slis)))
           (tol (s) (coerce s 'list)))
      (let* ((len (* (+ w h) 2))
             (s (fills s len)))
        (format t "~{~a~%~}"
                `(,(subseq s 0 (1+ w))
                  ,@(mapcar (lambda (l) (coerce l 'string))
                            (apply #'mapcar #'list
                                   `(,(reverse (tol (subseq s (+ w h w 1) len)))
                                     ,@(loop
                                          :for i :from 0 :upto (- w 2)
                                          :collect (tol (fills "　" (1- w))))
                                     ,(tol (subseq s (1+ w) (+ w h))))))
                  ,(reverse (subseq s (+ w h) (+ w h w 1)))))))))

(defun clam-eval (args &optional environment)
  (print environment)
  (let* ((args (coerce args 'list))
         (ret t)
         (built-in-command (get-command (intern (string-downcase (first args)) :keyword))))
    (if built-in-command
        (setf ret (apply built-in-command (rest args)))
        (format *standard-output* "~a"
                (with-output-to-string (out)
                  (uiop:run-program `(,(first args) ,@(rest args))
                                    :output out
                                    :error-output out))))
    ret))

(defun clam-print (object)
  (unless (eq object +clam-status-exit+)
    (unless (eq object +clam-status-do-nothing+)
      (format *standard-output* "~s" object)
      (finish-output *standard-output*)))
  object)

(defun clam-init ()
  (let ((environment nil))
    (setf (getf environment :SHELL) "clam")
    (setf (getf environment :USER) "cl-user")
    (setf (getf environment :HOME) (user-homedir-pathname))
    (setf (getf environment :PWD) (uiop:getcwd))
    (setf (getf environment :PATH) '(#P"/bin/"))
    environment))

(defun clam-shell ()
  (let ((environment (clam-init)))
    (loop
       :for status := (handler-case
                          (clam-print (clam-eval (clam-read) environment))
                        (end-of-file ()
                          (return-from clam-shell)))
       :until (eq status +clam-status-exit+))))
