#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defun newlines-until-pos (file position)
  (with-open-file (stream file)
    (1+ (loop until (>= (file-position stream) position)
              count (char= (read-char stream) #\Newline)))))

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

(defun find-definition-in-file (call file)
  (with-open-file (stream file)
    (loop with min = ()
          for pos = (file-position stream)
          for top = (read-toplevel-form stream)
          while top
          do (let ((searchpos (search (princ-to-string call) top :test #'char-equal)))
               (when (and searchpos (or (not min) (<= searchpos (third min))))
                 (setf min (list pos top searchpos))))
          finally (return (values (first min) (second min))))))
