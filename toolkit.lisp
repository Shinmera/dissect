#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defun read-source-form (file start)
  (ignore-errors
   (with-open-file (stream file)
     (file-position stream start)
     (read stream))))

(defun read-source-form-at-line (file line)
  (ignore-errors
   (with-open-file (stream file)
     (loop for char = (read-char stream NIL NIL)
           while char
           do (when (char= #\Newline char)
                (decf line))
              (when (= line 0)
                (return (read stream)))))))

(defun newlines-until-pos (file position)
  (ignore-errors
   (with-open-file (stream file)
     (1+ (loop until (>= (file-position stream) position)
               count (char= (read-char stream) #\Newline))))))

(defun read-toplevel-form (stream)
  (loop for char = (read-char stream NIL NIL)
        while char until (char= char #\())
  (when (peek-char NIL stream NIL NIL)
    (with-output-to-string (output)
      (write-char #\( output)
      (loop with level = 1
            for char = (read-char stream NIL NIL)
            while char until (<= level 0)
            do (write-char char output)
               (case char
                 (#\( (incf level))
                 (#\) (decf level)))))))

(defun %print-as-hopefully-in-source (stream thing &rest arg)
  (declare (ignore arg))
  (write-string (print-as-hopefully-in-source thing) stream))

(defun print-as-hopefully-in-source (thing)
  (typecase thing
    (symbol (symbol-name thing))
    (string (prin1-to-string thing))
    (list (format NIL "(~{~/dissect::%print-as-hopefully-in-source/~^ ~})" thing))
    (T (princ-to-string thing))))

(defun find-definition-in-file (call file)
  (let ((definition (print-as-hopefully-in-source call)))
    (with-open-file (stream file)
      (loop with min = ()
            for pos = (file-position stream)
            for top = (read-toplevel-form stream)
            while top
            do (let ((searchpos (search definition top :test #'char-equal)))
                 (when (and searchpos (or (not min) (<= searchpos (third min))))
                   (setf min (list pos top searchpos))))
            finally (return (values (first min) (second min)))))))

(defun chop-stack (stack)
  "Look for stack truncations and cappings and chop it down accordingly."
  (flet ((frame= (frame func)
           (or (eql (call frame) func)
               (eql (call frame) (fdefinition func)))))
    (loop with start = 0
          for i from 0
          for frame in stack
          until (frame= frame 'stack-truncator)
          collect frame into final-stack
          do (when (frame= frame 'stack-capper)
               (setf start (1+ i)))
          finally (return (nthcdr start final-stack)))))

;; Copied over from bordeaux-threads.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro (require :smputil)
  #+corman  (require :threads))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or armedbear
        (and allegro multiprocessing)
        (and clisp mt)
        (and openmcl openmcl-native-threads)
        (and cmu mp)
        corman
        (and ecl threads)
        mkcl
        lispworks
        (and digitool ccl-5.1)
        (and sbcl sb-thread)
        scl)
  (pushnew :thread-support *features*))

(defun current-thread ()
  (or 
   (when (find-package :bt) (funcall (find-symbol (string :current-thread) :bt)))
   #+thread-support
   (or
    #+abcl (threads:current-thread)
    #+allegro mp:*current-process*
    #+clisp (mt:current-thread)
    #+ccl ccl:*current-process*
    #+cmucl mp:*current-process*
    #+corman threads:*current-thread*
    #+ecl mp::*current-process*
    #+lispworks (mp:get-current-process)
    #+digitool ccl:*current-process*
    #+mkcl mt::*thread*
    #+sbcl sb-thread:*current-thread*
    #+scl thread:*thread*
    NIL)))
