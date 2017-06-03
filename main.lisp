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

(defvar *clam-built-in-commands*
  `(("exit" ,#'(lambda (&rest args) nil))))

(defun clam-eval (args)
  (let* ((args (coerce args 'list))
         (ret t)
         (built-in-command (find (first args) *clam-built-in-commands* :test #'string= :key #'first)))
    (if built-in-command
        (progn
          (format t "built-in: ~s ~s~%" built-in-command (rest args))
          (setf ret (apply (second built-in-command) (rest args))))
        (format t "external: ~a ~s~%" (first args) (rest args)))
    ret))

(defun clam-print (object)
  (when object
    (format *standard-output* "~s" object)
    (finish-output *standard-output*)
    object))

(defun clam-loop ()
  (loop
     :for status := (clam-print (clam-eval (clam-read)))
     :while status))
