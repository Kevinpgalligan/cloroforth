;;;; Shared implementation between Forths, regardless of
;;;; compilation target.

(defpackage :cloroforth.def
  (:use :cl)
  (:documentation "Stores the declarations/definitions of words."))

(in-package cloroforth.defs)

(defparameter *codewords* (list))
(defparameter *words* (list))

(defun add-codeword! (sym)
  (let* ((name (symbol-name sym))
         (existing
           (find sym *codewords* :key #'symbol-name :test #'string=)))
    (when (null existing)
      ;; Doesn't matter what order we store codewords. I don't think
      ;; they depend on each other, as they are defined directly
      ;; in machine code / whatever.
      (push (intern name) *codewords*))))

(defun make-word (name words)
  (list (intern name)
        (mapcar (lambda (w) (intern (symbol-name w)))
                words)))

(defun add-word! (sym words)
  (let* ((name (symbol-name sym))
         (existing (find name *words* :key #'symbol-name)))
    (when (null existing)
      (setf *words* (append *words* (list (make-word name words)))))))

(defun get-definition (word)
  (or (second (find word *words* :key #'first :test 'eq))
      (error ("Word not defined."))))


(defpackage :cloroforth.core
  (:use :cl)
  (:documentation "Core functionality shared between targets of Forth compilation."))

(in-package cloroforth.core)

(defmacro defcodeword (name)
  `(cloroforth.defs:add-codeword! ,name))

(defmacro defword (name &body words)
  `(cloroforth.defs:add-word! ,name ,words))
