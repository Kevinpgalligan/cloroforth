(defpackage :cloroforth-asd
  (:use :cl :asdf))

(in-package :cloroforth-asd)

(defsystem cloroforth
  :license "MIT"
  :author "Kevin Galligan"
  :description "A Forth interpreter."
  :depends-on (:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "core")
               ))
