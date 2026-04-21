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

;; Parameter stack, in user-accessible memory.
(defun fpush (value)
  ;; Parameter stack grows towards low memory.
  (decf *sp* *cell-size*)
  (fwrite-cell! *sp* value))

(defun fpop ()
  (when (>= *sp* *base*)
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
;; 1 byte for flags (currently just immediate mode flag) + length, name, address of
;; next entry, codeword id (1 byte), and a blob of data.
(defclass dictionary-entry ()
  ((name :initarg :name :reader name)
   (codeword-id :initarg :codeword-id :reader codeword-id)
   (immediate? :initarg :immediate? :reader immediate?)))

(defun make-dictionary-entry (name codeword-id immediate?)
  (make-instance 'dictionary-entry
                 :name name
                 :codeword-id codeword-id
                 :immediate? immediate?))

(defun write-dictionary-entry! (mem address cell-size prev-entry-address entry)
  (setf (aref mem address)
        (logxor (if (immediate? entry) (ash 1 7) 0)
                (length (name entry))))
  (incf address)
  ;; Max length of word names will depend on the number of flags squashed into
  ;; the length byte, currently it's just 1.
  (when (>= (length (name entry)) (ash 1 7))
    (error "Word name is too long."))
  (loop for c across (name entry)
        do (progn
             (setf (aref mem address) (char-code c))
             (incf address)))
  (write-cell! mem address cell-size prev-entry-address)
  (incf address cell-size)
  (setf (aref mem address) (codeword-id entry))
  (incf address)
  address)

(defun read-dictionary-entry (mem address cell-size)
  (let* ((b (aref mem address))
         (name-length (logandc2 b (ash 1 7)))
         (immediate? (not (= b name-length)))
         (name (coerce (loop for i from (1+ address) below (+ address 1 name-length)
                             collect (code-char (aref mem i)))
                       'string)))
    (incf address (+ 1 name-length)) ; skip length byte + name
    (let ((link (read-cell mem address cell-size)))
      (incf address cell-size)
      (let ((codeword-id (aref mem address)))
        (values
         (make-instance 'dictionary-entry :immediate? immediate? :name name :codeword-id codeword-id)
         link)))))

(when nil

;;; The codewords!
  (defparameter *codewords* nil)

  (defclass codeword-def ()
    ((name :initarg :name :reader name)
     (implementation-fun :initarg :implementation-fun :accessor implementation-fun)))

  (defun make-codeword-def (name f)
    (make-instance 'codeword-def
                   :name name
                   :implementation-fun f))

  ;; TODO: make this flexible enough that it can accept args
  ;;       like documentation. I wanna generate the documentation.
  ;; TODO: codewords are written into dict in opposite order.
  (defmacro defcode (name &body body)
    `(let ((cw (make-codeword-def
                ,name
                (lambda ()
                  ,@body
                  (fnext)))))
       (let ((existing (find ,name *codewords* :key #'name :test #'string=)))
         (if existing
             (setf (implementation-fun existing) (implementation-fun cw))
             (push cw *codewords*)))
       ;; This lets us define new codewords, or redefine them, as the Forth
       ;; interpreter is running.
       (when (and (boundp '*state*) (not (null *state*)))
         (load-codeword cw))))

  (defmacro defop (op-name op-fun)
    `(defcode ,op-name
         (fpush (,op-fun (fpop) (fpop)))))
  (defop "+" +)
  (defop "-" -)
  (defop "*" *)
  (defop "/" floor)

  (defcode "dup"
      (fpush (fmemget (1+ *sp*))))

  (defcode "drop"
      (fpop))

  (defcode "swap"
      (let ((a (fpop))
            (b (fpop)))
        (fpush a)
        (fpush b)))

  (defcode "."
      (format t "~a~%" (fpop)))

  (defcode "docol"
      (fpush-ret *ip*)
    ;; Implicitly allowing fnext to increment the instruction pointer.
    ;; This idea is used in various places.
    (setf *ip* (fmemget-pointer *ip*)))
  

  (defcode "exit"
      (setf *ip* (fpop-ret)))

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
