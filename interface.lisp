#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(declaim (ftype (function () list) stack restarts)
         (notinline stack restarts))
(defun stack ())

(defun restarts ())

(declaim (notinline stack-truncator))
(defun stack-truncator (function)
  (funcall function))

(defmacro with-truncated-stack (() &body body)
  `(stack-truncator (lambda () ,@body)))

(declaim (notinline stack-capper))
(defun stack-capper (function)
  (funcall function))

(defmacro with-capped-stack (() &body body)
  `(stack-capper (lambda () ,@body)))

(defun present (thing &optional (destination T))
  (with-capped-stack ()
    (etypecase destination
      ((eql T) (present thing *standard-output*))
      ((eql NIL) (with-output-to-string (stream)
                   (present thing stream)))
      (stream (present-object thing destination)))))

(defgeneric present-object (thing stream))

(defmethod present-object ((condition condition) stream)
  (format stream "~a" condition)
  (format stream "~&   [Condition of type ~s]" (type-of condition))
  (format stream "~&~%")
  (present-object T stream))

(defmethod present-object ((thing (eql T)) stream)
  (present-object (capture-environment) stream))

(defmethod present-object ((list list) stream)
  (when list
    (etypecase (first list)
      (restart (format stream "~&Available restarts:")
       (loop for i from 0
             for item in list
             do (format stream "~& ~d: " i)
                (present-object item stream)))
      (call (format stream "~&Backtrace:")
       (loop for item in list
             do (format stream "~& ")
                (present-object item stream))))))

(defclass restart ()
  ((name :initarg :name :reader name)
   (report :initarg :report :reader report)
   (restart :initarg :restart :reader restart)
   (object :initarg :object :reader object)
   (interactive :initarg :interactive :reader interactive)
   (test :initarg :test :reader test)))

(defmethod print-object ((restart restart) stream)
  (print-unreadable-object (restart stream :type T)
    (format stream "[~s] ~s"
            (name restart) (report restart))))

(defmethod present-object ((restart restart) stream)
  (format stream "[~a] ~a" (name restart) (report restart)))

(defgeneric invoke (restart &rest args))

(defmethod invoke ((restart restart) &rest args)
  (if (restart restart)
      (apply (restart restart) args)
      (apply #'invoke-restart (name restart) args)))

(defclass unknown-arguments ()
  ())

(defmethod print-object ((args unknown-arguments) stream)
  (format stream "#<Unknown Arguments>"))

(defclass unavailable-argument ()
  ())

(defmethod print-object ((arg unavailable-argument) stream)
  (format stream "#<Unavailable>"))

(defclass call ()
  ((pos :initarg :pos :reader pos)
   (call :initarg :call :reader call)
   (args :initarg :args :reader args)
   (file :initarg :file :reader file)
   (line :initarg :line :reader line)
   (form :initarg :form :reader form)))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type T)
    (format stream "[~a] ~a~@[ | ~a~@[:~a~]~]"
            (pos call) (call call) (file call) (line call))))

(defmethod present-object ((call call) stream)
  (let ((*print-pretty* NIL)
        (*print-readably* NIL)
        (args (args call)))
    (format stream "~d: ~:[(~s ~s)~;(~s~{ ~a~})~]"
            (pos call)
            ;; If args is a list then they will be listed
            ;; separated by spaces.
            (listp args)
            (call call)
            (if (listp args)
                (loop for arg in args
                      collect (or (ignore-errors (princ-to-string arg))
                                  "<error printing arg>"))
                args))))

(defclass environment ()
  ((condition :initarg :condition :reader environment-condition)
   (stack :initarg :stack :reader environment-stack)
   (restarts :initarg :restarts :reader environment-restarts)
   (thread :initarg :thread :reader environment-thread))
  (:default-initargs
   :condition NIL
   :stack (stack)
   :restarts (restarts)
   :thread (current-thread)))

(declaim (inline capture-environment))
(defun capture-environment (&optional condition)
  (with-capped-stack ()
    (make-instance 'environment :condition condition)))

(defmethod present-object ((env environment) stream)
  (with-slots ((condition condition) (stack stack) (restarts restarts) (thread thread)) env
    (format stream "~a" env)
    (format stream "~&   [Environment~@[ of thread ~a~]]" thread)
    (when condition
      (format stream "~&~%")
      (format stream "~a" condition)
      (format stream "~&   [Condition of type ~s]" (type-of condition)))
    (when restarts
      (format stream "~&~%")
      (present-object restarts stream))
    (when stack
      (format stream "~&~%")
      (present-object stack stream))))
