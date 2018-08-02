#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defclass abcl-call (call)
  ((frame :initarg :frame :accessor frame)))

(defun resolve-file-slots (call)
  (multiple-value-bind (file line form) (source-location (frame call))
    (setf (slot-value call 'file) file
          (slot-value call 'line) line
          (slot-value call 'form) form))
  call)

(macrolet ((define-resolvent (name)
             `(defmethod ,name ((call abcl-call))
                (unless (slot-boundp call ',name)
                  (resolve-file-slots call))
                (call-next-method))))
  (define-resolvent file)
  (define-resolvent line)
  (define-resolvent form))

(defun function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defgeneric source-location (object))

(defmethod source-location ((symbol symbol))
  (when (pathnamep (ext:source-pathname symbol))
    (let* ((file (ext:source-pathname symbol))
          (pos (ext:source-file-position symbol))
          (exists (probe-file file)))
      (values file
              (when exists (newlines-until-pos file pos))
              (when exists (read-source-form file pos))))))

(defmethod source-location ((frame sys::java-stack-frame))
  (destructuring-bind (&key class method file line) (sys:frame-to-list frame)
    (declare (ignore method))
    (let ((file (or (find-file-in-path file *source-path*)
                    (let ((f (format nil "~{~a/~}~a"
                                     (butlast (split-string class "\\."))
                                     file)))
                      (find-file-in-path f *source-path*)))))
      (and file
           (values file line)))))

(defmethod source-location ((frame sys::lisp-stack-frame))
  (let ((operator (first (sys:frame-to-list frame))))
    (etypecase operator
      (list nil)
      (function (source-location operator))
      (symbol (source-location operator)))))

(defmethod source-location ((fun function))
  (let ((name (function-name fun)))
    (and name (source-location name))))

(defun translate-class-name (symbol)
  ())

(defun system-property (name)
  (java:jstatic "getProperty" "java.lang.System" name))

(defun pathname-parent (pathname)
  (make-pathname :directory (butlast (pathname-directory pathname))))

(defun pathname-absolute-p (pathname)
  (eq (car (pathname-directory pathname)) ':absolute))

(defun split-string (string regexp)
  (coerce
   (java:jcall (java:jmethod "java.lang.String" "split" "java.lang.String") string regexp)
   'list))

(defun path-separator ()
  (java:jfield "java.io.File" "pathSeparator"))

(defun search-path-property (prop-name)
  (let ((string (system-property prop-name)))
    (and string (remove nil (mapcar #'truename (split-string string (path-separator)))))))

(defun jdk-source-path ()
  (let* ((jre-home (truename (system-property "java.home")))
         (src-zip (merge-pathnames "src.zip" (pathname-parent jre-home)))
         (truename (probe-file src-zip)))
    (and truename (list truename))))

(defun class-path ()
  (append (search-path-property "java.class.path")
          (search-path-property "sun.boot.class.path")))

(defvar *source-path*
  (append (search-path-property "user.dir")
          (jdk-source-path))
  "List of directories to search for source files.")

(defun zipfile-contains-p (zipfile-name entry-name)
  (let ((zipfile (java:jnew (java:jconstructor "java.util.zip.ZipFile" "java.lang.String") zipfile-name)))
    (java:jcall (java:jmethod "java.util.zip.ZipFile" "getEntry" "java.lang.String") zipfile entry-name)))

(defun find-file-in-path (filename path)
  (labels ((try (dir)
             (cond ((not (pathname-type dir))
                    (let ((f (probe-file (merge-pathnames filename dir))))
                      (and f `(:file ,(namestring f)))))
                   ((equal (pathname-type dir) "zip")
                    (try-zip dir))
                   (t (error "strange path element: ~s" path))))
           (try-zip (zip)
             (let* ((zipfile-name (namestring (truename zip))))
               (and (zipfile-contains-p zipfile-name filename)
                    `(:dir ,zipfile-name  ,filename)))))
    (cond ((pathname-absolute-p filename) (probe-file filename))
          (t
           (loop for dir in path
                 if (try dir) return it)))))

(defun make-call (i frame)
  (destructuring-bind (function . args) (sys:frame-to-list frame)
    (make-instance
     'abcl-call
     :pos i
     :call function
     :args args
     :frame frame)))

(defun stack ()
  (chop-stack
   (loop for frame in (cddr (sys:backtrace))
         for i from 0
         collect (make-call i frame))))

(defclass abcl-restart (restart)
  ((interactive :initarg :interactive :accessor interactive)
   (test :initarg :test :accessor test)))

(defun make-restart (restart)
  (make-instance
   'abcl-restart
   :name (system::restart-name restart)
   :restart (system::restart-function restart)
   :report (let* ((*print-readably* NIL)
                  (report (system::restart-report-function restart)))
             (typecase report
               (function (with-output-to-string (stream)
                           (funcall report stream)))
               (T report)))
   :interactive (system::restart-interactive-function restart)
   :test (system::restart-test-function restart)
   :object restart))

(defun restarts ()
  (mapcar #'make-restart (compute-restarts)))
