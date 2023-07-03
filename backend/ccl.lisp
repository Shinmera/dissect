(in-package #:org.tymoonnext.dissect)

(defclass ccl-call (call)
  ((source-note :initarg :source-note :accessor source-note)))

(defun resolve-file-slots (call)
  (let* ((source-note (source-note call))
         (file (ccl:source-note-filename source-note))
         (pos (ccl:source-note-start-pos source-note)))
    (setf (slot-value call 'line) (when (and file pos) (newlines-until-pos file pos))
          (slot-value call 'form) (when (and file pos) (read-source-form file pos))))
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
         (source-note (ccl:function-source-note function))
         (args (ccl:frame-supplied-arguments
                pointer context :unknown-marker (make-instance 'unavailable-argument)))
         (args (if (listp args) args (make-instance 'unknown-arguments))))
    (make-instance
     'ccl-call
     :pos i
     :call (or (ccl:function-name function) function)
     :args args
     :locals (loop for (name . value) in (ccl:frame-named-variables pointer context)
                   collect (cons name value))
     :file (when (ccl:source-note-filename source-note)
             (translate-logical-pathname (ccl:source-note-filename source-note)))
     :source-note source-note)))

(setf (fdefinition 'stack)
      (lambda ()
        (let ((i 0)
              (stack ()))
          (ccl:map-call-frames
           #'(lambda (pointer context)
               (push (make-call i pointer context) stack)
               (incf i))
           :start-frame-number 1)
          (chop-stack (nreverse stack)))))

(defclass ccl-restart (restart)
  ())

(defun make-restart (restart)
  (make-instance
   'ccl-restart
   :name (ccl::%restart-name restart)
   :restart (ccl::%restart-action restart)
   :report (let* ((*print-readably* NIL)
                  (report (ccl::%restart-report restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :interactive (ccl::%restart-interactive restart)
   :test (ccl::%restart-test restart)
   :object restart))


(setf (fdefinition 'restarts)
      (lambda (&optional condition)
        (mapcar #'make-restart (compute-restarts condition))))
