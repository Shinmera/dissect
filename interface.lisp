#|
 This file is a part of Dissect
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass restart ()
  ((name :initarg :name :accessor name)
   (report :initarg :report :accessor report)
   (restart :initarg :restart :accessor restart)
   (object :initarg :object :accessor object)))

(defmethod print-object ((restart restart) stream)
  (print-unreadable-object (restart stream :type T)
    (format stream "[~s] ~a"
            (name restart) (report restart))))

(defclass call ()
  ((pos :initarg :pos :accessor pos)
   (call :initarg :call :accessor call)
   (args :initarg :args :accessor args)
   (file :initarg :file :accessor file)
   (line :initarg :line :accessor line)))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type T)
    (format stream "[~a] ~a~@[ | ~a~@[:~a~]~]"
            (pos call) (call call) (file call) (line call))))

(defun stack ())

(defun restarts ())
