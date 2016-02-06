;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:github-repl
  :description "Github REPL to interact with the API"
  :author "sergiobuj"
  :license "MIT"
  :depends-on (#:yason
               #:cl-github-v3
               #:drakma)
  :serial t
  :components ((:file "package")
               (:file "github-repl")))

(asdf:defsystem #:github-repl-test
  :description "Tests for github-repl"
  :author "sergiobuj"
  :license "MIT"
  :depends-on (#:fiveam
               #:github-repl)
  :serial t
  :pathname "test/"
  :components ((:file "github-repl-test"))
  :perform (test-op (o s)
            (uiop:symbol-call :fiveam '#:run! :github-repl)))

