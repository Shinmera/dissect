#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:dissect
  (:use #:cl)
  (:nicknames #:org.tymoonnext.dissect)
  (:shadow #:restart)
  ;; interface.lisp
  (:export
   #:stack
   #:restarts
   #:with-truncated-stack
   #:with-capped-stack
   #:present
   #:present-object
   
   #:restart
   #:name
   #:report
   #:restart
   #:object
   #:invoke

   #:unknown-arguments
   #:unavailable-argument
   
   #:call
   #:pos
   #:call
   #:args
   #:file
   #:line
   #:form

   #:environment
   #:environment-condition
   #:environment-stack
   #:environment-restarts
   #:environment-thread
   #:capture-environment))
