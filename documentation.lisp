#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (type var doc) in pairs
             collect `(setf (documentation ',var ',type) ,doc))))

(setdocs
  (function stack
   "Returns a list of CALL objects describing the stack from the point where this function was called.

This excludes the call to STACK itself.
Any calls above a WITH-CAPPED-STACK form, and below a WITH-TRUNCATED-STACK
form are also excluded.

Returns an empty list on unsupported platforms.

See CALL
See WITH-TRUNCATED-STACK
See WITH-CAPPED-STACK")
  
  (function restarts
   "Returns a list of RESTART objects describing the currently available restarts.

Returns an empty list on unsupported platforms.

See RESTART")

  (function with-truncated-stack
    "Calls BODY in an environment where a call to STACK will not report frames further down.

See STACK")
  
  (function with-capped-stack
    "Calls BODY in an environment where a call to STACK will not report frames further up.

See STACK")

  (function present
   "Prints a neat representation of THING to DESTINATION.

DESTINATION can be one of the following types:
  (eql NIL)  --- The representation is printed and returned as a string.
  (eql T)    --- The representation is printed to *STANDARD-OUTPUT*.
  STREAM     --- The representation is printed to the stream.

THING can be one of the following types:
  RESTART      --- Restarts are presented as:
                     [NAME] REPORT
  CALL         --- Calls are presented as:
                     POS (CALL ARGS..)
  ENVIRONMENT  --- Environments are presented as a multiline description
                   of all the parts it references (condition, stack,
                   restarts, thread).
  CONDITION    --- Conditions are presented as:
                     CONDITION
                       [Condition of type TYPE]
  (EQL T)      --- Presents the environment at point using
                   CAPTURE-ENVIRONMENT.
  LIST         --- The list can contain either restarts or calls. In both
                   cases the behaviour is to output a header line, followed
                   by the presentation of each item in the list on its own
                   line.

Internally the function PRESENT-OBJECT is used to perform the actual
printing.

See RESTART
See CALL
See ENVIRONMENT
See CONDITION
See CAPTURE-ENVIRONMENT
See PRESENT-OBJECT")

  (function present-object
   "Internal generic function for pretty printing. 

See PRESENT"))

(setdocs
  (type restart
   "Class container for restart information.

See NAME
See REPORT
See RESTART
See OBJECT
See INTERACTIVE
See TEST
See INVOKE")
  
  (function name
   "Returns the restart's symbol. Use this for INVOKE-RESTART.

See RESTART")

  (function report
   "Returns the report string describing the restart's effects.

See RESTART")

  (function restart
   "Returns a symbol to the restart-function or a direct function-object.

See RESTART")

  (function object
   "Returns the platform-internal restart object.

See RESTART")

  (function interactive
   "Returns the interactive restart function.

See RESTART")

  (function test
   "Returns the restart test function.

See RESTART")

  (function invoke
   "Invoke the restart that the restart object references.

See RESTART"))

(setdocs
  (type unknown-arguments
   "Used to represent an unknown list of arguments.

Instances of this class are printed as #<Unknown Arguments>")
  
  (type unavailable-argument
   "Used to represent an argument that isn't available in the environment.

Instances of this class are printed as #<Unavailable>"))

(setdocs
  (type call
   "Class container for stack frame information.

See POS
See CALL
See ARGS
See FILE
See LINE
See FORM")
  
  (function pos
   "Returns the position of the call on the stack.

See CALL")
  
  (function call
   "Returns the stack call function.

Can be either a function object or the name of a global function.

See CALL")
  
  (function args
   "Returns a list of arguments that were used in the frame call.

If the arguments list is not available, this may also return an instance
of UNKNOWN-ARGUMENTS. The values in the list may be instances of
UNAVAILABLE-ARGUMENT if the argument is unknown or could not be captured
for some reason.

See UNKNOWN-ARGUMENTS
See UNAVAILABLE-ARGUMENT
See CALL")
  
  (function file
   "If possible, returns the file the called function is defined in.

See CALL")
  
  (function line
   "If possible, returns the line number in the file where the function is defined.

See CALL")
  
  (function form
   "If possible, returns the actual definition form of the function.

See CALL"))

(setdocs
  (type environment
   "Container class for a current \"environment\".

An instance of this class is intended to represent most of the runtime
environment present at a particular point. It is useful for stashing away
debug information for inspection at a later date.

See CAPTURE-ENVIRONMENT
See ENVIRONMENT-CONDITION
See ENVIRONMENT-STACK
See ENVIRONMENT-RESTARTS
See ENVIRONMENT-THREAD")

  (function environment-condition
   "Returns the condition stored in the environment (if any).

See CL:CONDITION
See ENVIRONMENT")

  (function environment-stack
   "Returns a list of calls stored in the environment (if any).

See CALL
See ENVIRONMENT")

  (function environment-restarts
   "Returns a list of restarts stored in the environment (if any).

See CL:RESTART
See ENVIRONMENT")

  (function environment-thread
   "Returns the thread stored in the environment (if any).

See SB-THREAD:THREAD
See THREADS:THREAD
See MP:PROCESS
See MT:THREAD
See CCL:PROCESS
See PROCESS:PROCESS
See THREAD:THREAD
See ENVIRONMENT")

  (function capture-environment
   "Capture the current environment into an environment object.

See ENVIRONMENT"))
