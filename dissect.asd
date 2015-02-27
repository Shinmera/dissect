#|
 This file is a part of Dissect
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem dissect
  :name "Dissect"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A small lib for introspecting the call stack and active restarts."
  :homepage "https://github.com/Shinmera/dissect"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "interface")
               #+abcl (:file "abcl")
               #+ccl (:file "ccl")
               #+ecl (:file "ecl")
               #+sbcl (:file "sbcl")
               (:file "documentation"))
  :depends-on ())
