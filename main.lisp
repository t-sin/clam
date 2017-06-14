(in-package :cl-user)
(defpackage :clam
  (:nicknames :clam/main)
  (:use :cl)
  (:import-from :clam/parse
                :clam-read)
  (:export :clam-shell))
(in-package :clam/main)


(defparameter *clam-environment* nil)

(defun clam-init ()
  (let ((environment nil))
    (setf (getf environment :SHELL) "clam")
    (setf (getf environment :USER) (sb-posix:getenv "USER"))
    (setf (getf environment :HOME) (user-homedir-pathname))
    (setf (getf environment :PWD) (sb-posix:getenv "PWD"))
    (setf (getf environment :PATH) '(#P"/bin/" #P"/usr/bin/"))
    environment))

(defun stringify-environment (env)
  (flet ((stringify (k v)
           (case k
             (:PATH (format nil "~{~a~^:~}" v))
             (t v))))
    (loop
       :for (k v) :on env :by #'cddr
       :collect (format nil "~a=~a"
                        (symbol-name k)
                        (stringify k v)))))

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

(defun search-command (command path)
  (if (probe-file command)
      (pathname command)
      (loop :named search
         :for dir :in path
         :for cmd-candidate := (merge-pathnames (make-pathname :name command) dir)
         :with cmdpath := nil
         :if (probe-file cmd-candidate)
         :do (setf cmdpath cmd-candidate)
         :finally (return-from search cmdpath))))

(defun clam-eval (args)
  (let* ((args (coerce args 'list))
         (ret t)
         (built-in-command (get-command (intern (string-downcase (first args)) :keyword))))
    (when args
      (if built-in-command
          (setf ret (apply built-in-command (rest args)))
          (let ((command (search-command (first args)
                                         (getf *clam-environment* :PATH))))
            (if command
                (format *standard-output* "~a"
                        (with-output-to-string (out)
                          (sb-ext:run-program command (rest args)
                                              :output out
                                              :error out
                                              :environment (stringify-environment *clam-environment*))))
                (format *standard-output* "~a: command not found~%" (first args))))))
    ret))

(defun clam-print (object)
  (unless (eq object +clam-status-exit+)
    (unless (eq object +clam-status-do-nothing+)
      (format *standard-output* "~s" object)
      (finish-output *standard-output*)))
  object)

(defun clam-shell ()
  (let ((*clam-environment* (clam-init)))
    (loop
       :for status := (handler-case
                          (clam-print (clam-eval (clam-read)))
                        (end-of-file ()
                          (return-from clam-shell)))
       :until (eq status +clam-status-exit+))))
