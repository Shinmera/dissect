(asdf:defsystem dissect
  :name "Dissect"
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A lib for introspecting the call stack and active restarts."
  :homepage "https://Shinmera.github.io/dissect/"
  :bug-tracker "https://github.com/Shinmera/dissect/issues"
  :source-control (:git "https://github.com/Shinmera/dissect.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "interface")
               (:module "backend"
                :components
                (#+abcl (:file "abcl")
                 #+allegro (:file "allegro")
                 #+ccl (:file "ccl")
                 #+clasp (:file "clasp")
                 #+clisp (:file "clisp")
                 #+ecl (:file "ecl")
                 #+sbcl (:file "sbcl")))
               (:file "documentation"))
  :depends-on (#+clisp :cl-ppcre))
