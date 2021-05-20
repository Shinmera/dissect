#|
 This file is part of Dissect
 Author: Bike <aeshtaer@gmail.com>
|#

(in-package #:org.tymoonnext.dissect)

(defun stack ()
  (let ((stack nil))
    (clasp-debug:map-indexed-backtrace
     (lambda (frame index)
       (let ((csl (clasp-debug:frame-function-source-position frame)))
         (push (make-instance 'call
                 :pos index
                 :call (or (clasp-debug:frame-function-name frame)
                           (clasp-debug:frame-function frame))
                 :args (clasp-debug:frame-arguments frame)
                 :form (clasp-debug:frame-function-form frame)
                 :file (and csl (clasp-debug:code-source-line-pathname csl))
                 :line (and csl (clasp-debug:code-source-line-line-number csl)))
               stack))))
    (nreverse stack)))

(defclass clasp-restart (restart)
  ((conditions :initarg :conditions :accessor conditions)))

(defun make-restart (restart)
  (make-instance 'clasp-restart
    :name (restart-name restart)
    :report (write-to-string restart :escape nil :readably nil)
    :restart (ext:restart-function restart)
    :object restart
    :interactive (ext:restart-interactive-function restart)
    :test (ext:restart-test-function restart)
    :conditions (ext:restart-associated-conditions restart)))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))

(defmacro with-capped-stack (&body body)
  `(clasp-debug:with-capped-stack () ,@body))

(defmacro with-truncated-stack (&body body)
  `(clasp-debug:with-truncated-stack () ,@body))
