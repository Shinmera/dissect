#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass ccl-call (call)
  ((source-note :initarg :source-note :accessor source-note)))

(defun read-source-form (file start)
  (ignore-errors
   (with-open-file (stream file)
     (file-position stream start)
     (read stream))))

(defun resolve-file-slots (call)
  (let ((source-note (source-note call)))
    (setf (line call) (newlines-until-pos (ccl:source-note-filename source-note)
                                          (ccl:source-note-start-pos source-note))
          (form call) (read-source-form (ccl:source-note-filename source-note)
                                        (ccl:source-note-start-pos source-note))))
  call)

(macrolet ((define-resolvent (name)
             `(defmethod ,name ((call ccl-call))
                (unless (slot-boundp call ',name)
                  (resolve-file-slots call))
                (call-next-method))))
  (define-resolvent line)
  (define-resolvent form))

(defun make-call (i pointer context)
  (let* ((function (ccl:frame-function pointer context))
         (source-note (ccl:function-source-note function)))
    (make-instance
     'ccl-call
     :pos i
     :call (or (ccl:function-name function) function)
     :args (ccl:frame-supplied-arguments pointer context :unknown-marker (make-instance 'unavailable-arg))
     :file (ccl:source-note-filename source-note)
     :source-note source-note)))

(defun stack ()
  (let ((i 0)
        (stack ()))
    (ccl:map-call-frames
     #'(lambda (pointer context)
         (push (make-call i pointer context) stack)
         (incf i))
     :start-frame-number 1)
    (nreverse stack)))

(defclass ccl-restart (restart)
  ((interactive :initarg :interactive :accessor interactive)
   (test :initarg :test :accessor test)))

(defun make-restart (restart)
  (make-instance
   'ccl-restart
   :name (ccl::%restart-name restart)
   :restart (ccl::%restart-action restart)
   :report (let ((report (ccl::%restart-report restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :interactive (ccl::%restart-interactive restart)
   :test (ccl::%restart-test restart)
   :object restart))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))
