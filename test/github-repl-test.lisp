;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defpackage :github-repl-test
  (:use :cl :fiveam))

(in-package :github-repl-test)

(def-suite :github-repl)
(in-suite :github-repl)

(test create-add-team-url
  (is (> 1 0)))
