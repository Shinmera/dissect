#|
 This file is a part of Dissect
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass ecl-call (call)
  ((file-pos :initarg :file-pos :accessor file-pos)))

(defun resolve-file-slots (call)
  (setf (slot-value call 'line) (when (file call) (newlines-until-pos (file call) (file-pos call)))
        (slot-value call 'form) (when (file call) (read-source-form (file call) (file-pos call))))
  call)

(macrolet ((define-resolvent (name)
             `(defmethod ,name ((call ecl-call))
                (unless (slot-boundp call ',name)
                  (resolve-file-slots call))
                (call-next-method))))
  (define-resolvent line)
  (define-resolvent form))

(defun function-name (function)
  (typecase function
    (generic-function (clos:generic-function-name function))
    (function (system:compiled-function-name function))))

(defun make-call (i function environment)
  (multiple-value-bind (file position) (system::bc-file function)
    (make-instance
     'ecl-call
     :pos i
     :call (typecase function
             (symbol function)
             (T (function-name function)))
     :args (let ((variables ())
                 (frame (si::decode-ihs-env environment)))
             (dolist (record (remove-if-not #'consp frame))
               (let* ((record0 (car record))
                      (record1 (cdr record)))
                 (when (or (symbolp record0) (stringp record0))
                   (push record1 variables))))
             variables)
     :file (when file (translate-logical-pathname file))
     :file-pos position)))

(defun stack ()
  (chop-stack
   (loop for ihs downfrom (1- (system::ihs-top)) above 0
         for i from 0
         collect (make-call
                  i
                  (system::ihs-fun ihs)
                  (system::ihs-env ihs)))))

(defclass ecl-restart (restart)
  ())

(defun make-restart (restart)
  (make-instance
   'ecl-restart
   :name (system::restart-name restart)
   :report (let* ((*print-readably* NIL)
                  (report (system::restart-report-function restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :restart (system::restart-function restart)
   :object restart
   :interactive (system::restart-interactive-function restart)
   :test (system::restart-test-function restart)))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))
