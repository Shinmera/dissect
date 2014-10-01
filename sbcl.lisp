#|
 This file is a part of Dissect
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass sbcl-call (call)
  ((info :initarg :info :accessor info)))

(defun frame-location (frame)
  (let* ((code-location (sb-di:frame-code-location frame))
         (debug-source (ignore-errors
                        (sb-di:code-location-debug-source code-location))))
    (when debug-source
      (let* ((form (sb-c::debug-source-form debug-source))
             (file (let ((file (sb-c::debug-source-namestring debug-source)))
                     (when (probe-file file)
                       file)))
             (line (when file
                     (multiple-value-bind (pos found-form) (find-definition-in-file (sb-debug::frame-call frame) file)
                       (when found-form (setf form found-form))
                       (when pos (newlines-until-pos file pos))))))
        (values file line form)))))

(defun make-call (frame)
  (multiple-value-bind (call args info) (sb-debug::frame-call frame)
    (multiple-value-bind (file line form) (frame-location frame)
      (make-instance
       'sbcl-call
       :pos (sb-di:frame-number frame)
       :call call
       :args args
       :info info
       :file file
       :line line
       :form form))))

(defun stack ()
  (loop for frame = (sb-di:frame-down (sb-di:top-frame))
        then (sb-di:frame-down frame)
        while frame
        collect (make-call frame)))

(defclass sbcl-restart (restart)
  ((interactive :initarg :interactive :accessor interactive)
   (test :initarg :test :accessor test)
   (conditions :initarg :conditions :accessor conditions)))

(defun make-restart (restart)
  (make-instance
   'sbcl-restart
   :name (restart-name restart)
   :report (with-output-to-string (stream)
             (funcall (sb-kernel::restart-report-function restart) stream))
   :restart (sb-kernel::restart-function restart)
   :object restart
   :interactive (sb-kernel::restart-interactive-function restart)
   :test (sb-kernel::restart-test-function restart)
   :conditions (sb-kernel::restart-associated-conditions restart)))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))
