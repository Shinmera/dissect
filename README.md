About Dissect <a href="https://travis-ci.org/Shinmera/dissect"><img src="https://travis-ci.org/Shinmera/dissect.svg?branch=master" alt="Build Status" align="right" /></a>
-------------
Dissect is a small library for introspecting the call stack and active restarts.

Supported Implementations
-------------------------
Since the call stack and restart inspection are not part of ANSI CL, pretty much all of Dissect is implementation-dependant. Currently the following implementations are supported:

* ABCL
* Allegro
* CCL
* CLISP
* ECL
* SBCL

How To
------
Retrieving a list of restart objects is done through `restarts`. Similarly, the current stack can be seen with `stack`. Returned by both are a list of objects, from which you can read out information. Depending on the implementation, additional slots may be included.

    (dissect:stack)
    ; => (#<CCL-CALL [0] CALL-CHECK-REGS | ccl:l1;l1-readloop.lisp.newest:827> #<CCL-CALL [1] CHEAP-EVAL | ...)
    
    (dissect:restarts)
    ; => (#<CCL-RESTART [SWANK::RETRY] "Retry SLIME REPL evaluation request."> #<CCL-RESTART [ABORT] ...)
    
    (dissect:form (first (dissect:stack)))
    ; => (DEFUN CALL-CHECK-REGS (FN &REST ARGS) ...)
    
    (dissect:restart (first (dissect:restarts)))
    ; => CCL:SIMPLE-RESTART

You can also get a fancy print of calls, restarts, conditions, or the current state using `present`:

    (dissect:present T)

    (handler-bind ((error #'dissect:present))
      (error "Hello!"))

Sometimes having the full stack shown gives you a lot of noise and uninteresting information. To limit this --and thus make the stacks dissect returns cleaner-- you can use `with-truncated-stack` and `with-capped-stack`. Those will ensure that only frames above and below the respective macros are shown. Similarly, those can easily lead to completely empty stack reports, so make sure to only use them where you are absolutely sure that you will not need the information anymore.

When you need to capture the current environment because for later processing, you can use `capture-environment`. This will return an object that contains the current stack, restarts, thread, and an optional condition object. Using this, the entire environment surrounding an error can be saved. `present` also works with an `environment` object.
