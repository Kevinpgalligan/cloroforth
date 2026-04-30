(defpackage :cloroforth.interpret
  (:use :cl)
  (:export
   :make-byte-array
   :init-forth
   :read-cell
   :write-cell!
   :fpush
   :fpop
   :fpush-ret
   :fpop-ret
   :read-dictionary-entry
   :write-dictionary-entry!
   :make-dictionary-entry
   :name
   :codeword-id
   :immediate?
   ))

(in-package cloroforth.interpret)

(defparameter *pad-offset* 340)

;;; System variables used by the interpreter.
(defparameter *ip* nil "Instruction pointer, holds memory address of a codeword token to execute.")
(defparameter *sp* nil "Stack pointer, holds memory address of the parameter stack.")
(defparameter *dp* nil "Dictionary pointer, next available cell in dictionary.")
(defparameter *base* nil "Address pointing to base of the parameter stack.")
(defparameter *return-stack* nil "Return stack, an actual CL data structure.")
(defparameter *mem* nil "The memory array.")
(defparameter *cell-size* nil "The number of bytes used by a single cell (a.k.a. 'machine word').")
(defparameter *int-upper-bound* nil "Upper bound on integers, given a cell size & the use of two's complement.")

(defun make-byte-array (size)
  (make-array size :element-type '(unsigned-byte 8)))

;;; Initialising the state of the interpreter.
;; Default system memory is currently ~2MB, should be enough to store the dictionary & parameter stack.
;; And 1MB for the user.
(defun init-forth (&key (cell-size 4) (max-line-size 1000)
                     (system-memory 2097152) (user-memory 1048576))
  (let ((total-memory (+ system-memory max-line-size user-memory)))
    (assert (> (expt 2 (* cell-size 8)) total-memory))
    (setf *cell-size* cell-size)
    (setf *int-upper-bound* (expt 2 (1- (* 8 *cell-size*))))
    (setf *sp* system-memory)
    (setf *dp* 0)
    (setf *base* system-memory)
    (setf *return-stack* nil)
    (setf *mem* (make-byte-array total-memory))))

;;; Memory utilities.
(defun read-cell (mem address cell-size)
  (loop with value = 0
        for i from address below (+ address cell-size)
        ;; Big-endian!
        do (setf value (+ (aref mem i) (* value 256)))
        finally (return value)))

(defun write-cell! (mem address cell-size value)
  (loop for i from (+ address cell-size -1) downto address
        do (progn
             (setf (aref mem i) (mod value 256))
             (setf value (ash value -8)))))

;;; Memory operations specific to the running interpreter, all prefixed with "f".
(defun fread-cell (address)
  (read-cell *mem* address *cell-size*))

(defun fwrite-cell! (address value)
  (write-cell! *mem* address *cell-size* value))

(defun fread-byte (address)
  (aref *mem* address))

(defun fwrite-byte (address value)
  (setf (aref *mem* address) value))

(defun fwrite-dictionary-byte! (value)
  (fwrite-byte *cp* value)
  (incf *cp*))

(defun fwrite-dictionary-cell! (value)
  (fwrite-cell! *cp* value)
  (incf *cp* *cell-size*))

;; Parameter stack, in user-accessible memory.
(defun fpush (value)
  ;; Parameter stack grows towards low memory.
  (decf *sp* *cell-size*)
  (fwrite-cell! *sp* value))

(defun fempty-stack? ()
  (>= *sp* *base*))

(defun fstack-size ()
  (floor (- *base* *sp*) *cell-size*))

(defun fpop ()
  (when (fempty-stack?)
    (error "Tried to pop from empty parameter stack!"))
  (let ((v (read-cell *mem* *sp* *cell-size*)))
    (fread-cell *sp*)
    (incf *sp* *cell-size*)
    v))

;; Return stack, not directly accessible to the user.
(defun fpush-ret (value)
  (push value *return-stack*))

(defun fpop-ret ()
  (when (null *return-stack*)
    (error "Tried to pop from empty return stack!"))
  (pop *return-stack*))

(defun fnext ()
  "Moves the instruction pointer forward."
  (incf *ip* *cell-size*))

;;; The dictionary.
(defconstant +immediate-flag+ (ash 1 7))

(defun fwrite-dictionary-entry! (name immediate? prev-entry-address codeword-id)
  ;; Max length of word names will depend on the number of flags squashed into
  ;; the length byte, currently it's just 1.
  (when (>= (length name) +immediate-flag+)
    (error "Word name is too long."))
  (fwrite-dictionary-byte!
   (logxor (if immediate? +immediate-flag+ 0)
           (length name)))
  (loop for c across name
        do (fwrite-dictionary-byte! (char-code c)))
  (fwrite-dictionary-cell! prev-entry-address)
  (fwrite-dictionary-byte! codeword-id))

;;; Defining codewords.
(defvar *codewords* (make-array 0 :fill-pointer t :adjustable t))

(defclass codeword-def ()
  ((name :initarg :name :reader name)
   (documentation :initarg :documentation :reader documentation)
   (implementation-fun :initarg :implementation-fun :accessor implementation-fun)))

(defmacro defcode (name params &body body)
  (alexandria:with-unique-names (cw existing)
    `(let ((,cw (make-codeword-def :name ,name
                                   :documentation ,(getf params :doc)
                                   :implementation-fun (lambda ()
                                                         ,@body
                                                         ;; Very important. After the execution of each word
                                                         ;; we increment the instruction pointer to move to
                                                         ;; the next word.
                                                         (fnext)))))
       ;; TODO need to write to the dictionary if there's a running Forth process.
       (let ((,existing (find ,name *codewords* :key #'name :test #'string=)))
         (if ,existing
             (setf
              (documentation existing) (documentation cw)
              (implementation-fun existing) (implementation-fun cw))
             (vector-push-extend cw *codewords*))))))

;;; The codewords themselves!
;; For user-defined words.
(defcode "docol"
    (:doc "For executing user-defined words.")
  (fpush-ret *ip*)
  (setf *ip* (fread-cell *ip*)))

(defcode "exit"
    (:doc "Used to return from user-defined words, by popping off the return stack.")
  (setf *ip* (fpop-ret)))

;; Basic manipulation of the parameter stack.
(defcode "drop"
    (:doc "Discards a value from the parameter stack.")
  (fpop))

(defcode "dup"
    (:doc "Duplicates the value on top of the parameter stack.")
  (when (fempty-stack?)
    (error "Tried to dup when parameter stack is empty."))
  ;; sp holds the address of the last value pushed to the stack.
  (fpush (fread-cell *sp*)))

(defcode "swap"
    (:doc "Swaps the two values at the top of the stack.")
  (when (< (fstack-size) 2)
    (error "Not enough values on parameter stack to swap."))
  (let ((a (fpop))
        (b (fpop)))
    (fpush a)
    (fpush b)))

(when nil

  (defmacro defop (op-name op-fun)
    `(defcode ,op-name
         (fpush (,op-fun (fpop) (fpop)))))
  (defop "+" +)
  (defop "-" -)
  (defop "*" *)
  (defop "/" floor)

  

  (defcode "."
      (format t "~a~%" (fpop)))
  

  (defcode "lit"
      (fpush (fmemget (1+ *ip*)))
    (incf *ip*))

  (defcode "!"
      (fmemset (fpop) (fpop)))
  (defcode "@"
      (fpush (fmemget (fpop))))

  (defcode ">R"
      (fpush-ret (fpop)))
  (defcode "R>"
      (fpush (fpop-ret)))
  (defcode "RSP!"
      (fmemset *rp* (fpop)))
  (defcode "RSP@"
      (fpush *rp*))
  (defcode "RSDROP"
      (fpop-ret)))
