(defpackage :cloroforth-test-asd
  (:use :cl :asdf))

(in-package :cloroforth-test-asd)

(defsystem cloroforth-test
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:cloroforth :fiveam)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "memory")
               ))
