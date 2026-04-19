(in-package cloroforth-test)

(def-suite memory
  :in cloroforth)

(in-suite memory)

(test read-and-write-cell
      (let ((xs (make-byte-array 10))
            (v 285)
            (addr 0)
            (cell-size 4))
        (write-cell! xs addr v cell-size)
        (format t "~%~a~%" xs)
        (is (= v (read-cell xs addr cell-size)))))
