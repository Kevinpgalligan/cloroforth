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

;;;; System variables used by the interpreter.
(defparameter *ip* nil "Instruction pointer, holds memory address of a codeword token to execute.")
(defparameter *sp* nil "Stack pointer, holds memory address of the top of the parameter stack.")
(defparameter *base* nil "Address pointing to base of the parameter stack.")
(defparameter *dp* nil "Dictionary pointer, holds memory address of last dictionary entry.")
(defparameter *here* nil "Next available cell in dictionary.")
(defparameter *tib* nil "Address pointing to start of the terminal input buffer.")
(defparameter *tib-tracker* nil "Data structure to track input in the TIB.")
(defparameter *return-stack* nil "Return stack, an actual CL data structure.")
(defparameter *mem* nil "The memory array.")
(defparameter *cell-size* nil "The number of bytes used by a single cell (a.k.a. 'machine word').")
(defparameter *int-upper-bound* nil "Upper bound on integers, given a cell size & the use of two's complement.")

(defun make-byte-array (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defclass tib-tracker ()
  ((addr :initarg :addr :accessor addr)
   (end-addr :initarg :end-addr :accessor end-addr)))

;;;; Initialising the state of the interpreter.
;; Default system memory is currently ~2MB, should be enough to store the dictionary & parameter stack.
;; And 1MB for the user.
(defun init-forth (&key (cell-size 4) (max-line-size 1000)
                     (system-memory 2097152) (user-memory 1048576)
                     (input-buffer-size 200))
  (let ((total-memory (+ system-memory max-line-size input-buffer-size user-memory)))
    (assert (> (expt 2 (* cell-size 8)) total-memory))
    (setf *cell-size* cell-size
          *int-upper-bound* (expt 2 (1- (* 8 *cell-size*)))
          *dp* 0
          *here* 0
          *sp* system-memory
          *base* system-memory
          *tib* system-memory
          *tib-tracker* (make-instance 'tib-tracker :addr 0 :end-addr 0)
          *return-stack* nil
          *mem* (make-byte-array total-memory))
    'ok))

;;;; Memory operations.
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

;; Note that any function prefixed with "f" assumes that global state has been initialised, and depends on it.
(defun fread-cell (address)
  (read-cell *mem* address *cell-size*))

(defun fwrite-cell! (address value)
  (write-cell! *mem* address *cell-size* value))

(defun fread-byte (address)
  (aref *mem* address))

(defun fwrite-byte (address value)
  (setf (aref *mem* address) value))

(defun fwrite-dictionary-byte! (value)
  (fwrite-byte *here* value)
  (incf *here*))

(defun fwrite-dictionary-cell! (value)
  (fwrite-cell! *here* value)
  (incf *here* *cell-size*))

(defun fread-string (address length)
  (coerce (loop for i from address below (+ address length)
                collect (code-char (fread-byte i)))
          'string))

(defun ftwos-complement (v)
  "Negative numbers are represented in the system via two's complement."
  (cond
    ((>= *int-upper-bound* v)
     (error "Value too high."))
    ((>= v 0)
     v)
    ;; Compare to the lower bound (maybe that should have its own variable).
    ((>= v (1- (- *int-upper-bound*)))
     ;; Map negative numbers -128,-127,...,-1 to 128,129,...,255.
     ;; (For the 1-byte case, but should be more general).
     (+ v (* 2 (1+ *int-upper-bound*))))
    (t
     (error "Value too low."))))

(defun fstring= (addr1 addr2 len)
  (loop repeat len
        for i from addr1
        for j from addr2
        when (not (= (fread-byte i) (fread-byte j)))
          return nil
        finally (return t)))

;;;; Parameter stack, in user-accessible memory.
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

(defun fstack-ref (index)
  (when (>= index (fstack-size))
    (error "Stack was smaller than expected."))
  (fread-cell (+ *sp* (* index *cell-size*))))

;;;; Return stack, not directly accessible to the user.
(defun fpush-ret (value)
  (push value *return-stack*))

(defun fpop-ret ()
  (when (null *return-stack*)
    (error "Tried to pop from empty return stack!"))
  (pop *return-stack*))

(defun fnext ()
  "Moves the instruction pointer forward."
  (incf *ip* *cell-size*))

;;;; The dictionary.
;; Dictionary entries follow the following format:
;;    link to previous entry (1 cell)
;;    length & flags         (1 byte)
;;    name                   (n bytes)
;;    codeword ID            (1 byte)
;;    data                   (n bytes)
;; A "cell" is what would normally be called a "word" when describing processor architecture, e.g. on a 64-bit
;; processor a word is 8 bytes. But Forth uses different terminology ("words" can be found in the "dictionary").
;; Also, in this Forth interpreter, the cell size is configurable.

(defconstant +immediate-flag+ (ash 1 7))

(defun fwrite-dictionary-entry! (name immediate? prev-entry-address codeword-id)
  ;; This is now the latest entry in the dictionary.
  (setf *dp* *here*)
  ;; Max length of word names will depend on the number of flags squashed into
  ;; the length byte, currently it's just 1.
  (when (>= (length name) +immediate-flag+)
    (error "Word name is too long."))
  (fwrite-dictionary-cell! prev-entry-address)
  (fwrite-dictionary-byte!
   (logxor (if immediate? +immediate-flag+ 0)
           (length name)))
  (loop for c across name
        do (fwrite-dictionary-byte! (char-code c)))
  (fwrite-dictionary-byte! codeword-id))

;;;; Defining codewords.
(defvar *codewords* (make-array 0 :fill-pointer t :adjustable t))

(defclass codeword-def ()
  ((name :initarg :name :reader name)
   (codeword-id :initarg :codeword-id :reader codeword-id)
   (docstring :initarg :docstring :accessor docstring)
   (implementation-fun :initarg :implementation-fun :accessor implementation-fun)))

(defmacro defcode (name params &body body)
  (alexandria:with-unique-names (cw existing)
    `(let ((,cw (make-instance 'codeword-def
                               :name ,name
                               :codeword-id (length *codewords*)
                               :docstring ,(getf params :doc)
                               :implementation-fun (lambda ()
                                                     ,@body
                                                     ;; Very important. After the execution of each word
                                                     ;; we increment the instruction pointer to move to
                                                     ;; the next word.
                                                     (fnext)))))
       (let ((,existing (find ,name *codewords* :key #'name :test #'string=)))
         (if ,existing
             (setf
              (docstring ,existing) (docstring ,cw)
              (implementation-fun ,existing) (implementation-fun ,cw))
             (progn
               (vector-push-extend ,cw *codewords*)
               (when *mem*
                 (fwrite-codeword-to-dictionary! ,cw))))))))

(defun fwrite-codeword-to-dictionary! (cw)
  (fwrite-dictionary-entry! (name cw) nil *dp* (codeword-id cw)))

;;;; The codewords themselves!
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

(defmacro defop (op-name op-fun)
  `(defcode ,op-name ()
     (fpush (mod (,op-fun (fpop) (fpop)) *int-upper-bound*))))
(defop "+" +)
(defop "-" -)
(defop "*" *)
(defop "/" floor)

(defcode "."
    (:doc "Pop a value off the parameter stack and print it.")
  (let ((v (fpop))
        (negative-cutoff (/ *int-upper-bound* 2)))
    (format t
            "~a~%"
            (if (< v negative-cutoff)
                v
                ;; Handling two's complement. Given 4 bits, the range of possible integers is
                ;; [-8, +7] (inclusive). In two's complement, the negative numbers are remapped to [+8, +15]
                ;; (that's a total of 8 negative values), where +8 is -8, +9 is -7, etc. In modulo arithmetic,
                ;; they behave the same as if they were negative. Anyway, here we need to map the range
                ;; [+8, +15] back to negative values.
                (- v *int-upper-bound*)))))

(defun fwrite-all-codewords-to-dictionary! ()
  (loop for cw across *codewords*
        for codeword-id from 0
        do (fwrite-codeword-to-dictionary! cw)))

(defun fdict-get-length (entry-addr)
  (logandc2 (fread-byte (+ entry-addr *cell-size*)) +immediate-flag+))

(defun fdict-get-name (entry-addr)
  (fread-string (fdict-get-name-addr entry-addr) (fdict-get-length entry-addr)))

(defun fdict-get-name-addr (entry-addr)
  (+ entry-addr *cell-size* 1))

(defun fdict-get-link (entry-addr)
  (fread-cell entry-addr))

(defun fdict-empty? ()
  (zerop *here*))

(defun ffind-word (name)
  (if (fdict-empty?)
      ;; Dictionary is empty.
      nil
      (loop with addr = *dp*
            do (cond
                 ((string= name (fdict-get-name addr))
                  (return addr))
                 ((zerop addr)
                  (return nil))
                 (t
                  (setf addr (fdict-get-link addr)))))))

(defun fread-line-into-tib ()
  (let ((line (or (read-line nil nil) "")))
    (loop for c across line
          for addr from *tib*
          do (fwrite-byte addr (char-code c)))
    (setf (addr *tib-tracker*) *tib*
          (end-addr *tib-tracker*) (+ *tib* (length line)))))

(defun fscan-tib-for-word ()
  "Moves forward scan address in the TIB, skipping whitespace, until a word is found.
Returns 3 values: whether a word was found (T/NIL), the word start address, and the length."
  (with-slots (addr end-addr) *tib-tracker*
    (loop while (and (< addr end-addr)
                     (is-whitespace? (code-char (fread-byte addr))))
          do (incf addr)
          finally (return (if (>= addr end-addr)
                              (values nil 0 0)
                              (values t
                                      addr
                                      (- (loop for i from (1+ addr)
                                               while (and (< i end-addr)
                                                          (not (is-whitespace? (code-char (fread-byte i)))))
                                               finally (return i))
                                         addr)))))))

(defun is-whitespace? (c)
  (member c '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return)))

(defcode "word"
    (:doc "Reads input into the TIB (terminal input buffer), skipping whitespace characters, until it finds a
word (sequence of non-whitespace characters). Leaves the address of this word, and its length, on the stack."
     :sig "-- addr n")
  (loop do (multiple-value-bind (found? addr length)
               (fscan-tib-for-word)
             (when found?
               (fpush addr)
               (fpush length)
               (return)))
           ;; Didn't find a word so need to read the next line.
        do (fread-line-into-tib)))

(defcode "find"
    (:doc "Takes a reference to a string (and its length) and returns dictionary address of a word of that name. Or, if
such a word is not found, returns -1."
     :sig "addr1 n -- addr2 | -1")
  ;; TODO can make this a macro, with-stack-vars, or maybe incorporate into defcode
  (let ((len (fpop))
        (str-addr (fpop)))
    (when (fdict-empty?)
      (fpush (ftwos-complement -1)))
    (loop with ptr = *dp*
          do (cond
               ((and (= len (fdict-get-length ptr))
                     (fstring= (fdict-get-name-addr ptr) str-addr len))
                (fpush ptr)
                (break))
               ;; TODO should probably wrap this in a function, it's repeated in ffind-word, the assumption
               ;; that the last (well, first) dictionary entry is at address 0.
               ((zerop ptr)
                (fpush (ftwos-complement -1))
                (break))
               (t
                (setf ptr (fdict-get-link ptr)))))))
