#|
 This file is a part of Dissect
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:dissect
  (:use #:cl)
  (:nicknames #:org.tymoonnext.dissect)
  (:shadow #:restart)
  ;; interface.lisp
  (:export
   #:restart
   #:name
   #:report
   #:restart
   #:object
   
   #:call
   #:pos
   #:call
   #:args
   #:file
   #:line
   
   #:stack
   #:restarts))
