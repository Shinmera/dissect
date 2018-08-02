#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass sbcl-call (call)
  ((info :initarg :info :accessor info)
   (frame :initarg :frame :accessor frame)))

(defun frame-location (frame)
  (let* ((code-location (sb-di:frame-code-location frame))
         (debug-source (ignore-errors
                        (sb-di:code-location-debug-source code-location))))
    (cond #+#.(cl:when (cl:find-symbol (cl:string 'core-debug-source-p) "SB-C") :sbcl)
          ((sb-c::core-debug-source-p debug-source)
           (values NIL NIL (sb-c::core-debug-source-form debug-source)))
          (debug-source
           (let* ((file (let ((file (sb-di:debug-source-namestring debug-source)))
                          (and file (probe-file file)))))
             (when file
               (multiple-value-bind (pos found-form)
                   (find-definition-in-file (sb-debug::frame-call frame) file)
                 (values file (newlines-until-pos file pos) found-form))))))))

(defun resolve-file-slots (call)
  (multiple-value-bind (file line form) (frame-location (frame call))
    (setf (slot-value call 'file) (when file (translate-logical-pathname file))
          (slot-value call 'line) line
          (slot-value call 'form) form))
  call)

(macrolet ((define-resolvent (name)
             `(defmethod ,name ((call sbcl-call))
                (unless (slot-boundp call ',name)
                  (resolve-file-slots call))
                (call-next-method))))
  (define-resolvent file)
  (define-resolvent line)
  (define-resolvent form))

(defun make-call (frame)
  (multiple-value-bind (call args info) (sb-debug::frame-call frame)
    (make-instance
     'sbcl-call
     :frame frame
     :pos (sb-di:frame-number frame)
     :call call
     :args args
     :info info)))

(defun stack ()
  (chop-stack
   (loop for frame = (sb-di:frame-down (sb-di:top-frame))
         then (sb-di:frame-down frame)
         while frame
         collect (make-call frame))))

(defclass sbcl-restart (restart)
  ((conditions :initarg :conditions :accessor conditions)))

(defun make-restart (restart)
  (make-instance
   'sbcl-restart
   :name (restart-name restart)
   :report (let* ((*print-readably* NIL)
                  (report (sb-kernel::restart-report-function restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :restart (sb-kernel::restart-function restart)
   :object restart
   :interactive (sb-kernel::restart-interactive-function restart)
   :test (sb-kernel::restart-test-function restart)
   :conditions (sb-kernel:restart-associated-conditions restart)))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))

(defun stack-capper (function)
  (declare (optimize (debug 3)))
  (funcall function))

(defun stack-truncator (function)
  (declare (optimize (debug 3)))
  (funcall function))

(defmacro with-truncated-stack (() &body body)
  `(stack-truncator (sb-int:named-lambda with-truncated-stack-lambda () ,@body)))

