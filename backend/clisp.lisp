#|
 This file is a part of Dissect
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass clisp-call (call)
  ((spec :initarg :spec :accessor spec)
   (frame-type :initarg :frame-type :accessor frame-type)))

(defmethod print-object ((call clisp-call) stream)
  (print-unreadable-object (call stream :type T)
    (format stream "[~a] ~s ~s~@[ | ~a~@[:~a~]~]"
            (pos call) (frame-type call) (call call) (file call) (line call))))

(defun resolve-file-slots (call)
  (let ((file (file call))
        (name (car (spec call)))
        (line (cdr (spec call))))
    (if file
        (multiple-value-bind (line form)
            (if line
                (values line (read-source-form-at-line file line))
                (multiple-value-bind (pos form) (find-definition-in-file file name)
                  (values (newlines-until-pos file pos) form)))
          (setf (slot-value call 'line) line)
          (setf (slot-value call 'form) form))
        (setf (slot-value call 'line) NIL
              (slot-value call 'form) NIL)))
  call)

(macrolet ((define-resolvent (name)
             `(defmethod ,name ((call clisp-call))
                (unless (slot-boundp call ',name)
                  (resolve-file-slots call))
                (call-next-method))))
  (define-resolvent line)
  (define-resolvent form))

(defun definition-file (name)
  (let* ((fspec (first (documentation name 'sys::file)))
         (file (if (consp fspec)
                   (second fspec)
                   fspec))
         (line (if (consp fspec)
                   (third fspec))))
    (when (and file (member (pathname-type file) custom:*compiled-file-types* :test #'equal))
      (setf file (loop for suffix in custom:*source-file-types*
                       thereis (probe-file (make-pathname :defaults file :type suffix)))))
    (when file
      (values (ignore-errors (truename file))
              (cons name line)))))

(defparameter *frame-prefixes*
  '(("\\[[0-9]+\\] frame binding variables" :bind-var)
    ("\\[[0-9]+\\] EVAL frame" :eval)
    ("\\[[0-9]+\\] compiled tagbody frame" :compiled-tagbody)
    ("\\[[0-9]+\\] compiled block frame" :compiled-block)
    ("\\[[0-9]+\\] unwind-protect frame" :unwind-protect)
    ("\\[[0-9]+\\] frame binding environments" :bind-env)
    ("\\[[0-9]+\\] catch frame" :catch)
    ("\\[[0-9]+\\] handler frame" :handler)
    ("<1(/[0-9]*)?> #<compiled-function" :compiled-fun)
    ("<1(/[0-9]*)?> #<system-function" :sys-fun)
    ("<1(/[0-9]*)?> #<special-operator" :special-op)
    ("<1(/[0-9]*)?> " :fun)
    ("<2(/[0-9]*)?> " :2nd-frame)
    ("APPLY frame" :apply)
    ("block frame" :block)
    ("nested block frame" :block)
    ("tagbody frame" :tagbody)
    ("nested tagbody frame" :tagbody)
    ("driver frame" :driver)
    ("CALLBACK frame" :callback)
    ("- " :stack-value)))

(defun starts-with-p (regexp string)
  (not (null (cl-ppcre:scan (concatenate 'string "^" regexp) string))))

(defun string-match (pattern string &optional (group 0))
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings pattern string))))
    (when match (elt match group))))

(defun trim-whitespace (string)
  (string-trim #(#\newline #\space #\tab) string))

(defun ensure-frame-string (frame)
  (etypecase frame
    (string frame)
    (#+NIL system::frame-pointer
     T
     ;; Note about the above kludge:
     ;; According to the hyperspec (typep o (type-of o))
     ;; always has to return true, but in the case of
     ;; frame-pointers and clisp, it always errors, saying
     ;; that it isn't a valid type specifier.
     ;; We fall back to just using describe-frame by
     ;; default, which is less nice.
     (with-output-to-string (stream)
       (sys::describe-frame stream frame)))))

(defmethod frame-type ((frame T))
  ;; again as above, we can't test better than this.
  (frame-type (ensure-frame-string frame)))

(defmethod frame-type ((frame string))
  (loop for (pattern type) in *frame-prefixes*
        do (when (starts-with-p pattern frame)
             (return type))))

(defun unneeded-frame-p (frame)
  (find (frame-type frame) '(:stack-value :bind-var :bind-env :compiled-tagbody :compiled-block)))

(defun split-frame-string (string)
  (let ((regex (format nil "~%\\(~{~A~^\\|~}\\)" (mapcar #'first *frame-prefixes*))))
    (loop for pos = 0 then (1+ (regexp:match-start match))
          for match = (regexp:match regex string :start pos)
          collect (if match
                      (subseq string pos (regexp:match-start match))
                      (subseq string pos))
          while match)))

(defun extract-function-name (frame)
  (let ((frame-string (ensure-frame-string frame)))
    (let ((first (first (split-frame-string frame-string))))
      (or (string-match (format nil "^<1(/[0-9]*)?>[ ~%]*#<[-A-Za-z]* (.*)>") first 1)
          (string-match (format nil "^<1(/[0-9]*)?>[ ~%]*(.*)") first 1)
          first))))

(defun extract-frame-line (frame)
  (let ((frame-string (ensure-frame-string frame)))
    (let ((name (case (frame-type frame-string)
                  ((:eval :special-op)
                   (string-match "EVAL frame .*?for form ([\\s\\S]*)" frame-string))
                  (:apply
                   (string-match "APPLY frame .*?for call ([\\s\\S]*)" frame-string))
                  ((:compiled-fun :sys-fun :fun)
                   (extract-function-name frame-string))
                  (:catch 'catch)
                  (:handler 'handler-bind)
                  (:unwind-protect 'unwind-protect)
                  (T frame-string))))
      (or (ignore-errors (read-from-string name)) name))))

(defun frame-venv (frame)
  (let ((env (sys::eval-at frame '(sys::the-environment))))
    (svref env 0)))

(defun next-venv (venv)
  (svref venv (1- (length venv))))

(defun venv-ref (env i)
  (let ((idx (* i 2)))
    (if (< idx (1- (length env)))
        (values (svref env idx) (svref env (1+ idx)))
        (venv-ref (next-venv env) (- i (/ (1- (length env)) 2))))))

(defun parse-stack-values (frame)
  (labels ((next (frame) (sys::frame-down 1 frame 1))
           (parse (frame accumulator)
             (let ((string (ensure-frame-string frame)))
               (case (frame-type frame)
                 (:stack-value
                  (parse (next frame) (cons string accumulator)))
                 (:fun
                  (dolist (string (rest (split-frame-string string)))
                    (when (eql (frame-type string) :stack-value)
                      (push string accumulator)))
                  (nreverse accumulator))
                 (T (parse (next frame) accumulator))))))
    (parse (next frame) ())))

(defun frame-var-count (frame)
  (cond ((sys::eval-frame-p frame)
         (loop for venv = (frame-venv frame) then (next-venv venv)
               while venv
               sum (/ (1- (length venv)) 2)))
        ((member (frame-type frame) '(:compiled-fun :sys-fun :fun :special-op))
         (length (parse-stack-values frame)))
        (T NIL)))

(defun frame-var-value (frame i)
  (cond ((sys::eval-frame-p frame)
         (let ((name (venv-ref (frame-venv frame) i)))
           (ignore-errors
            (sys::eval-at frame name))))
        ((member (frame-type frame) '(:compiled-fun :sys-fun :fun :special-op))
         (let ((str (nth i (parse-stack-values frame))))
           (trim-whitespace (subseq str 2))))))

(defun make-call (i frame)
  (let* ((type (frame-type frame))
         (call (extract-frame-line frame))
         (args (case type
                 ((:eval :apply)
                  (when (listp call) (cdr call)))
                 (T (or (let ((count (frame-var-count frame)))
                          (when count
                            (loop for i from 0 below count
                                  collect (frame-var-value frame i))))
                        (make-instance 'unknown-arguments)))))
         (call (if (and (find type '(:eval :apply)) (listp call))
                   (first call)
                   call)))
    (multiple-value-bind (file spec) (definition-file call)
      (make-instance
       'clisp-call
       :pos i
       :call call
       :args args
       :frame-type type
       :file file
       :spec spec))))

(defun stack ()
  (chop-stack
   (let ((mode 2 #|all-stack-elements|#))
     (loop with i = -1
           for last = NIL then frame
           for frame = (sys::the-frame)
           then (sys::frame-up 1 frame mode)
           until (eq frame last)
           unless (unneeded-frame-p frame)
           collect (make-call (incf i) frame)))))

;;;;;
;; Restarts

(defclass clisp-restart (restart)
  ())

(defun make-restart (restart)
  (make-instance
   'clisp-restart
   :name (system::restart-name restart)
   :report (let* ((*print-readably* NIL)
                  (report (system::restart-report restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :restart (system::restart-invoke-function restart)
   :object restart
   :interactive (system::restart-interactive restart)
   :test (system::restart-test restart)))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))
