About Dissect
-------------
Dissect is a small library for introspecting the call stack and active restarts.

Supported Implementations
-------------------------
Since the call stack and restart inspection are not part of ANSI CL, pretty much all of Dissect is implementation-dependant. Currently the following implementations are supported:

* SBCL
* CCL

How To
------
Retrieving a list of restart objects is done through `restarts`. Similarly, the current stack can be seen with `stack`. Returned by both are a list of objects, from which you can read out information. Depending on the implementation, additional slots may be included.

    (dissect:stack)
    => (#<CCL-CALL [0] CALL-CHECK-REGS | ccl:l1;l1-readloop.lisp.newest:827> #<CCL-CALL [1] CHEAP-EVAL | ...)
    
    (dissect:restarts)
    => (#<CCL-RESTART [SWANK::RETRY] "Retry SLIME REPL evaluation request."> #<CCL-RESTART [ABORT] ...)
    
    (dissect:form (first (dissect:stack)))
    => (DEFUN CALL-CHECK-REGS (FN &REST ARGS) ...)
    
    (dissect:restart (first (dissect:restarts)))
    => CCL:SIMPLE-RESTART

