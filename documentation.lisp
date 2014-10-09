#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.dissect)

(defmacro docfun (funcname format-string &rest format-args)
  `(setf (documentation ',funcname 'function)
         (format NIL ,format-string ,@format-args)))

(docfun name "Returns the restart's symbol. Use this for INVOKE-RESTART.")
(docfun report "Returns the report string describing the restart's effects.")
(docfun restart "Returns a symbol to the restart-function or a direct function-object.")
(docfun object "Returns the platform-internal restart object.")

(docfun pos "Returns the position of the call on the stack.")
(docfun call "Returns the stack call function.")
(docfun args "Returns a list of arguments that were passed. The arguments may or may not be actually usable values.")
(docfun file "If possible, returns the file the called function is defined in.")
(docfun line "If possible, returns the line number in the file where the function is defined.")
(docfun form "If possible, returns the actual definition form of the function.")

(docfun stack "Returns a list of CALL objects describing the stack from the point where this function was called, excluding the call to STACK itself.

Returns an empty list on unsupported platforms.")
(docfun restarts "Returns a list of RESTART objects describing the currently available restarts.

Returns an empty list on unsupported platforms.")
