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
   #:locals
   #:file
   #:line
   #:form

   #:environment
   #:environment-condition
   #:environment-stack
   #:environment-restarts
   #:environment-thread
   #:capture-environment))
