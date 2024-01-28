(defpackage :cloroforth.interpret
  (:use :cl))

;; TODO:
;;    - write function definitions for low-level words.
;;       (defcode pop ...))
;;    - Later: implement the ':' word and execute it on each of
;;      the words, it's more flexible. And allows e.g. changing
;;      the compiler-adapting words.

(in-package cloroforth.interpret)

(defparameter *s* nil)

(defclass forth-state ()
  ((stack :initarg :stack :accessor stack)
   (dictionary :initarg :dictionary :accessor dictionary)))

(defun make-dictionary-entry (word fun)
  (list word fun))

(defun dictionary-add (state word fun)
  (with-slots (dictionary) state
    (push (make-dictionary-entry word fun))))

(defun dictionary-lookup (state word)
  (with-slots (dictionary) state
    (or (second (find word dictionary :key #'first))
        (error (format nil "Missing dictionary entry for '~a'." word)))))

(defun make-forth-state (&key (dictionary-bytes 32768))
  (make-instance 'forth-state
                 :stack nil
                 :dictionary nil))

(defun make-word-implementation (state word)
  ;; Resolve words at compile time so that redefining a word
  ;; doesn't mess up other words that depended on it.
  ;; TODO: could maybe compile the lambda?
  ;; TODO: make this a word, based on lower-level primitives.
  (let ((funs
          (loop for subword in (cloroforth.def:get-definition word)
                collect (dictionary-lookup state subword))))
    (lambda (s)
      (loop for fun in funs
            do (funcall fun s)))))

