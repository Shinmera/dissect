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
   #:restart
   #:name
   #:report
   #:restart
   #:object

   #:unknown-arguments
   #:unavailable-argument
   
   #:call
   #:pos
   #:call
   #:args
   #:file
   #:line
   #:form
   
   #:stack
   #:restarts
   #:present))
