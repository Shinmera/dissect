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

(defun debug-var-info (var)
  (let ((s (find-symbol "DEBUG-VAR-INFO" :sb-di)))
    (when (and s (fboundp s))
      (funcall s var))))

(defun frame-locals (frame)
  (let* ((all-vars (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))
         (loc (sb-di:frame-code-location frame))
         ;; FIXME: Is discarding invalid vars necessary? Is there any
         ;; use in them?
         (vars (remove-if (lambda (var)
                            (ecase (sb-di:debug-var-validity var loc)
                              (:valid nil)
                              ((:invalid :unknown) t)))
                          all-vars))
         (more-context (find :more-context vars :key #'debug-var-info))
         (more-count (find :more-count vars :key #'debug-var-info)))
    (when vars
      (append
       (loop for var across vars
             collect (cons
                      (sb-di:debug-var-symbol var)
                      (sb-di:debug-var-value var frame)))
       (when (and more-context more-count)
         (list (cons 'sb-debug::more
                     (multiple-value-list
                      (sb-c:%more-arg-values (sb-di:debug-var-value more-context frame)
                                             0
                                             (sb-di:debug-var-value more-count frame))))))))))

(defun make-call (frame)
  (multiple-value-bind (call args info) (sb-debug::frame-call frame)
    (make-instance
     'sbcl-call
     :frame frame
     :pos (sb-di:frame-number frame)
     :call call
     :args args
     :locals (frame-locals frame)
     :info info)))

(setf (fdefinition 'stack)
      (lambda ()
        (chop-stack
         (loop for frame = (or (sb-debug::resolve-stack-top-hint)
                               (sb-di:frame-down (sb-di:top-frame)))
                 then (sb-di:frame-down frame)
               while frame
               collect (make-call frame)))))

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

(setf (fdefinition 'restarts)
      (lambda (&optional condition)
        (mapcar #'make-restart (compute-restarts condition))))

(setf (fdefinition 'stack-capper)
      (lambda (function)
        (declare (optimize (debug 3)))
        (funcall function)))

(setf (fdefinition 'stack-truncator)
      (lambda (function)
        (declare (optimize (debug 3)))
        (funcall function)))

(defmacro %with-truncated-stack (() &body body)
  `(stack-truncator (sb-int:named-lambda with-truncated-stack-lambda () ,@body)))
(setf (macro-function 'with-truncated-stack) (macro-function '%with-truncated-stack))
