(defpackage :cloroforth.interpret
  (:use :cl))

(in-package cloroforth.interpret)

;;; Global vars used for easy access to the attributes of the
;;; currently-used Forth state.
(defparameter *state* nil)
(defparameter *ip* nil "Instruction pointer.")
(defparameter *sp* nil "Stack pointer.")
(defparameter *dp* nil "Dictionary pointer.")
(defparameter *here* nil "First free address after dictionary.")
(defparameter *ret* nil "Return stack.")
(defparameter *mem* nil "Memory array.")

;;; Small utility class for the return stack.
(defclass stack ()
  ((data :initarg :data :accessor data)))
(defun make-stack ()
  (make-instance 'stack :data nil))
(defun stack-push (s item) (push item (data s)))
(defun stack-pop (s) (pop (data s)))

;;; Forth state.
(defclass forth-state ()
  ((ip :initarg :ip :accessor ip)
   (dp :initarg :dp :accessor dp)
   (here :initarg :here :accessor here)
   (sp :initarg :sp :accessor sp)
   (retstack :initarg :retstack :accessor retstack)
   (memory :initarg :memory :accessor memory)
   (dict :initform nil :accessor dict)))

(defun make-forth-state (&key (memory-size 1048576))
  (make-instance 'forth-state
                 :ip 0
                 :dp 0
                 :here 0
                 :sp (1- memory-size)
                 :retstack (make-stack)
                 :memory (make-array memory-size)))

(defclass dict-entry ()
  ((name :initarg :name :reader name)
   (start-addr :initarg :start-addr :reader start-addr)
   (name-addr :initarg :name-addr :reader name-addr)
   (codeword-addr :initarg :codeword-addr :reader codeword-addr)
   (data-addr :initarg :data-addr :reader data-addr)))

(defun make-dict-entry (name start-addr)
  ;; 1 cell for pointer to the next dictionary
  ;; entry and 1 cell for the flags/length.
  (let* ((name-addr (+ 2 start-addr))
         (codeword-addr (+ name-addr (length name))))
    (make-instance 'dict-entry
                   :name name
                   :start-addr start-addr
                   :name-addr name-addr
                   :codeword-addr codeword-addr
                   :data-addr (1+ codeword-addr))))

(defun dictionary-lookup (name)
  (find name (dict *state*) :key #'name :test #'string=))

(defmacro with-forth (state &body body)
  (alexandria:once-only (state)
    `(let ((*state* ,state)
           (*ip* (ip ,state))
           (*dp* (dp ,state))
           (*here* (here ,state))
           (*sp* (sp ,state))
           (*ret* (retstack ,state))
           (*mem* (memory ,state)))
       ,@body)))


;;; Next, low-level primitives used for interacting with the
;;; active Forth state.

;;; Push and pop to/from the Forth parameter stack.
(defun fpush (x)
  (setf (aref *mem* *sp*) x)
  (decf *sp*))
(defun fpop ()
  (if (>= *sp* (length *mem*))
      (error "Stack is empty.")
      (progn
        (incf *sp*)
        (aref *mem* *sp*))))

;;; Convenience functions for memory.
(defun fmemget (addr)
  "Gets the value at ADDR in memory."
  (aref *mem* addr))
(defun fmemset (addr value)
  (setf (aref *mem* addr) value))
(defun fmemget-pointer (addr)
  "Gets the value *pointed to* by the value at ADDR in memory."
  (fmemget (fmemget addr)))
(defun fdictwrite (x)
  "Writes a value to the dictionary and increases the dictionary pointer."
  (fmemset *here* x)
  (incf *here*))

(defun fnext ()
  (incf *ip*))


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
    ;; TODO: when it's actually ready to run by itself.
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
