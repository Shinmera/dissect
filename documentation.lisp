#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  (stack
   "Returns a list of CALL objects describing the stack from the point where this function was called, excluding the call to STACK itself.
Returns an empty list on unsupported platforms.")
  
  (restarts
   "Returns a list of RESTART objects describing the currently available restarts.
Returns an empty list on unsupported platforms.")

  (with-truncated-stack
      "Calls BODY in an environment where a call to STACK will not report frames further down.")
  
  (with-capped-stack
      "Calls BODY in an environment where a call to STACK will not report frames further up.")

  (present
   "Prints a neat representation of THING to DESTINATION.

DESTINATION can be NIL, T, or a STREAM.
THING can be a list of either RESTARTs or CALLs, a  RESTART, a CALL, an ENVIRONMENT, a CONDITION, or T.
In the last case, the current ENVIRONMENT is presented.")

  (present-object
   "Internal generic function for pretty printing. See PRESENT."))

(setdocs
  ((restart type)
   "Class container for restart information.")
  
  (name
   "Returns the restart's symbol. Use this for INVOKE-RESTART.")

  (report
   "Returns the report string describing the restart's effects.")

  (restart
   "Returns a symbol to the restart-function or a direct function-object.")

  (object
   "Returns the platform-internal restart object.")

  (interactive
   "Returns the interactive restart function.")

  (test
   "Returns the restart test function."))

(setdocs
  ((unknown-arguments type)
   "Used to represent an unknown list of arguments.")
  ((unavailable-argument type)
   "Used to represent an argument that isn't available in the environment."))

(setdocs
  ((call type)
   "Class container for stack call information.")
  
  (pos
   "Returns the position of the call on the stack.")
  
  (call
   "Returns the stack call function.")
  
  (args
   "Returns a list of arguments that were passed or an instance of UNKNOWN-ARGUMENTS. The arguments may or may not be actually usable values.")
  
  (file
   "If possible, returns the file the called function is defined in.")
  
  (line
   "If possible, returns the line number in the file where the function is defined.")
  
  (form
   "If possible, returns the actual definition form of the function."))

(setdocs
  ((environment type)
   "Container class for a current \"environment\". Has slots for the stack, restarts, and a condition.")

  (environment-condition
   "Returns the condition stored in the environment (if any).")

  (environment-stack
   "Returns a list of calls stored in the environment (if any).")

  (environment-restarts
   "Returns a list of restarts stored in the environment (if any).")

  (environment-thread
   "Returns the thread stored in the environment (if any).")

  (capture-environment
   "Capture the current environment into an environment object."))
