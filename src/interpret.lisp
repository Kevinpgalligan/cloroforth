(defpackage :cloroforth.interpret
  (:use :cl))

(in-package cloroforth.interpret)

(defparameter *pad-offset* 340)

;;; System variables used by the interpreter.
(defparameter *ip* nil "Instruction pointer, holds memory address of a codeword token to execute.")
(defparameter *sp* nil "Stack pointer, holds memory address of the parameter stack.")
(defparameter *base* nil "Address pointing to base of the parameter stack.")
(defparameter *return-stack* nil "Return stack, an actual CL data structure.")
(defparameter *mem* nil "The memory array.")
(defparameter *cell-size* nil "The number of bytes used by a single cell (a.k.a. 'machine word').")

(defun make-byte-array (size)
  (make-array size :element-type '(unsigned-byte 8)))

;;; Initialising the state of the interpreter.
;; Default system memory is currently ~2MB, should be enough to store the dictionary & parameter stack.
(defun init-forth (&key (cell-size 4) (max-line-size 1000)
                     (system-memory 2097152) (user-memory 1048576))
  (let ((total-memory (+ system-memory max-line-size user-memory)))
    (assert (> (expt 2 (* cell-size 8)) total-memory))
    (setf *cell-size* cell-size)
    (setf *sp* system-memory)
    (setf *base* system-memory)
    (setf *rp* nil)
    (setf *mem* (make-byte-array total-memory))))

;;; Memory utilities.
;; TODO Need to handle negative numbers, i.e. twos complement.
(defun read-cell (mem address cell-size)
  (loop with value = 0
        for i from address below (+ address cell-size)
        ;; Big-endian!
        do (setf value (+ (aref mem i) (* value 256)))
        finally return value))

(defun write-cell! (mem address value cell-size)
  (loop for i from (+ address cell-size -1) downto address
        do (progn
             ;; TODO I think there's a way to get the quotient and remainder with
             ;; one function call & multiple-value-bind.
             (setf (aref mem i) (mod value 256))
             (setf value (rem value 256)))))

;;; Memory operations specific to the running interpreter, all prefixed with "f".
(defun fread-cell (address)
  (read-cell *mem* address *cell-size*))

(defun fwrite-cell! (address value)
  (write-cell! *mem* address value *cell-size*))

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
    (incf *sp* *cell-size*)))

;; Return stack, not directly accessible to the user.
(defun fpush-ret (value)
  (push *return-stack*))

(defun fpop-ret (value)
  (when (null *return-stack*)
    (error "Tried to pop from empty return stack!"))
  (pop *return-stack*))

(defun fnext ()
  "Moves forward the instruction pointer."
  (incf *ip* *cell-size*))


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
  (fpop-ret))


;;; The interpreter setup and loop.
(defun interpret ()
  (setf *words* nil)
  (with-forth (make-forth-state)
    (bootstrap)
    (loop do (let* ((word (read-word))
                    (entry (dictionary-lookup word)))
               (if entry
                   (funcall (fmemget (codeword-addr entry)))
                   (fpush (parse-integer word)))))
    ;; TODO: uncomment when it's actually ready to run by itself.
    ;;(word-loop)
    ))

(defparameter *words* nil)
(defun read-word ()
  (loop while (null *words*)
        do (setf *words* (str:split #\space (read-line))))
  (pop *words*))

(defun bootstrap ()
  (load-dictionary)
  ;; TODO uncomment when I've defined QUIT
  #+nil(let ((quit-entry (or (dictionary-lookup "quit")
                        (error "No 'quit' word in dictionary."))))
         (setf *ip* (codeword-addr quit-entry)))
  )

(defun load-dictionary ()
  (loop for cw in *codewords*
        do (load-codeword cw))
  ;; TODO: load the words defined in Forth itself.
  )

(defun load-codeword (cw)
  (let ((old-dp *dp*))
    ;; Update dictionary pointer to point to this new word.
    (setf *dp* *here*)
    ;; Write link to previous dictionary entry.
    (fdictwrite old-dp)
    ;; TODO: maybe consider flags. Also need to enforce
    ;; the max length.
    (fdictwrite (length (name cw)))
    (loop for c across (name cw)
          do (fdictwrite (char-code c)))
    (fdictwrite (implementation-fun cw)))
  (push (make-dict-entry (name cw) *dp*)
        (dict *state*)))

(defun word-loop ()
  "Basically runs Forth words in a loop. Assumes that a Forth state has been
bound and that all the bootstrapping has been done: dictionary loaded into
memory, instruction pointer pointing at the appropriate memory location, etc."
  (loop do (funcall (fmemget-pointer *ip*))))
