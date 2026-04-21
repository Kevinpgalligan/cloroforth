(in-package cloroforth-test)

(def-suite memory
  :in cloroforth)

(in-suite memory)

(test read-and-write-cell
  (let ((xs (make-byte-array 10))
        (v 285)
        (addr 0)
        (cell-size 4))
    (write-cell! xs addr cell-size v)
    (is (= v (read-cell xs addr cell-size)))))

(test push-and-pop
  (init-forth)
  (let ((v1 12345)
        (v2 54321))
    (fpush v1)
    (fpush v2)
    (is (= v2 (fpop)))
    (is (= v1 (fpop)))))

(test pop-when-empty-stack
  (init-forth)
  (fiveam:signals error
    (fpop)))

(test push-pop-return-stack
  (init-forth)
  (let ((v1 123456789)
        (v2 888888888))
    (fpush-ret v1)
    (fpush-ret v2)
    (is (= v2 (fpop-ret)))
    (is (= v1 (fpop-ret)))))

(test pop-ret-when-empty
  (init-forth)
  (fiveam:signals error
    (fpop-ret)))

(test dictionary-serialisation
  (let ((mem (make-byte-array 10000))
        (d1 (make-dictionary-entry "hello" 0 nil))
        (d2 (make-dictionary-entry "goodbye" 1 t)))
    (let ((d2-addr (write-dictionary-entry! mem 0 2 0 d1)))
      (write-dictionary-entry! mem d2-addr 2 0 d2)
      (let ((re-d1 (read-dictionary-entry mem 0 2))
            (re-d2 (read-dictionary-entry mem d2-addr 2)))
        (is (string= (name d1) (name re-d1)))
        (is (= (codeword-id d1) (codeword-id re-d1)))
        (is (eq (immediate? d1) (immediate? re-d1)))
        (is (string= (name d2) (name re-d2)))
        (is (= (codeword-id d2) (codeword-id re-d2)))
        (is (eq (immediate? d2) (immediate? re-d2)))))))
