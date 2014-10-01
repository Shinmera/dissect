#|
 This file is a part of Dissect
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass ccl-call (call)
  ())

(defun read-source-form (file start)
  (ignore-errors
   (with-open-file (stream file)
     (file-position stream start)
     (read stream))))

(defun make-call (i pointer context)
  (multiple-value-bind (function pc) (ccl::cfp-lfun pointer)
    ;; No idea what PC actuall is. Welp.
    (let ((source-note (ccl:function-source-note function)))
      (make-instance
       'ccl-call
       :pos i
       :call (ccl::lfun-name function)
       :args (mapcar #'cdr (ccl::arguments-and-locals context pointer function pc))
       :file (ccl:source-note-filename source-note)
       :line (newlines-until-pos (ccl:source-note-filename source-note)
                                 (ccl:source-note-start-pos source-note))
       :form (read-source-form (ccl:source-note-filename source-note)
                               (ccl:source-note-start-pos source-note))))))

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
