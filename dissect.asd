#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem dissect
  :name "Dissect"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A lib for introspecting the call stack and active restarts."
  :homepage "https://github.com/Shinmera/dissect"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "interface")
               (:module "backend"
                :components
                (#+abcl (:file "abcl")
                 #+allegro (:file "allegro")
                 #+ccl (:file "ccl")
                 #+clisp (:file "clisp")
                 #+ecl (:file "ecl")
                 #+sbcl (:file "sbcl")))
               (:file "documentation"))
  :depends-on (#+clisp :cl-ppcre))
