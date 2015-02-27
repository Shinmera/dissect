#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass restart ()
  ((name :initarg :name :accessor name)
   (report :initarg :report :accessor report)
   (restart :initarg :restart :accessor restart)
   (object :initarg :object :accessor object))
  (:documentation "Class container for restart information."))

(defmethod print-object ((restart restart) stream)
  (print-unreadable-object (restart stream :type T)
    (format stream "[~s] ~s"
            (name restart) (report restart))))

(defclass unknown-arguments ()
  ())

(defmethod print-object ((args unknown-arguments) stream)
  (format stream "#<Unknown Arguments>"))

(defclass unavailable-argument ()
  ())

(defmethod print-object ((arg unavailable-argument) stream)
  (format stream "#<Unavailable>"))

(defclass call ()
  ((pos :initarg :pos :accessor pos)
   (call :initarg :call :accessor call)
   (args :initarg :args :accessor args)
   (file :initarg :file :accessor file)
   (line :initarg :line :accessor line)
   (form :initarg :form :accessor form))
  (:documentation "Class container for stack call information."))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type T)
    (format stream "[~a] ~a~@[ | ~a~@[:~a~]~]"
            (pos call) (call call) (file call) (line call))))

(defun stack ())

(defun restarts ())

(defgeneric present (thing &optional stream)
  (:method ((condition condition) &optional (stream T))
    (format stream "~a" condition)
    (format stream "~&   [Condition of type ~s]" (type-of condition))
    (format stream "~&~%")
    (present T stream))
  (:method ((thing (eql T)) &optional (stream T))
    (present (restarts) stream)
    (format stream "~&~%")
    (present (stack) stream))
  (:method ((list list) &optional (stream T))
    (when list
      (etypecase (first list)
        (restart (format stream "~&Available restarts:")
         (loop for i from 0
               for item in list
               do (format stream "~& ~d: " i)
                  (present item stream)))
        (call (format stream "~&Backtrace:")
         (loop for item in list
               do (format stream "~& ")
                  (present item stream))))))
  (:method ((restart restart) &optional (stream T))
    (format stream "[~a] ~a" (name restart) (report restart)))
  (:method ((call call) &optional (stream T))
    (let ((*print-pretty* NIL))
      (format stream "~d: ~:[~s ~s~;(~s~{ ~s~})~]"
              (pos call) (listp (args call)) (call call) (args call)))))
