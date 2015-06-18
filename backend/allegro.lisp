#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass acl-call (call)
  ())

(defun fspec-definition-location (fspec)
  (if (and (listp fspec) (eq (car fspec) :internal))
      (fspec-definition-locations (second fspec))
      (let ((defs (excl::find-source-file fspec)))
        (when (and (null defs) (listp fspec) (string= (car fspec) '#:method))
          ;; If methods are defined in a defgeneric form, the source location is
          ;; recorded for the gf but not for the methods. Therefore fall back to
          ;; the gf as the likely place of definition.
          (setf defs (excl::find-source-file (second fspec))))
        (third (car defs)))))

(defun make-call (i frame)
  (make-instance
   'acl-call
   :pos i
   :call (xref::object-to-function-name (debugger:frame-function frame))
   :args (loop for i from 0 below (debugger:frame-number-vars frame)
               collect (debugger:frame-var-value frame i))
   :file (fspec-definition-location (debugger:frame-function frame))
   :line NIL))

(defun next-frame (frame)
  (let ((next (excl::int-next-older-frame frame)))
    (and next (if (debugger:frame-visible-p next)
                  next (next-frame next)))))

(defun top-frame ()
  (let ((magic-symbol (make-symbol "FOO"))
        (top-frame (excl::int-newest-frame (excl::current-thread))))
    (loop for frame = top-frame then (next-frame frame)
          repeat 30
          while frame
          do (when (eq (debugger:frame-name frame) magic-symbol)
               (return (next-frame frame)))
          finally (return top-frame))))

(defun stack ()
  (chop-stack
   (loop for frame = (next-frame (next-frame (top-frame)))
         then (next-frame frame)
         for i from 0
         while frame
         collect (make-call i frame))))

(defclass acl-restart (restart)
  ())

(defun make-restart (restart)
  (make-instance
   'acl-restart
   :name (excl::restart-name restart)
   :restart (excl::restart-function restart)
   :report (let* ((*print-readably* NIL)
                  (report (excl::restart-report-function restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :interactive (excl::restart-interactive-function restart)
   :test (excl::restart-test-function restart)
   :object restart))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))
