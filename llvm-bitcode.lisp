;;;; llvm-bitcode.lisp

(in-package #:llvm-bitcode)

(quasiquote-2.0:enable-quasiquote-2.0)
(cl-interpol:enable-interpol-syntax)

;;; "llvm-bitcode" goes here. Hacks and glory await!

(defconstant bits-in-byte 8)

(defparameter abbrev-id-width 2)

(defparameter abbrev-record-handlers (make-hash-table :test #'equal))

(define-condition llvm-bitcode-read-error (error simple-condition) ())

(defmacro with-yield-dispatch ((&rest handlers) &body body)
  "We redefine YIELD so that it smartly handles special KWD arguments"
  `(macrolet ((yield! (form)
		`(dispatch (yield ,form)))
	      (dispatch (form)
		(alexandria:with-gensyms (g!-it)
		  `(let ((,g!-it ,form))
		     (iter (while t)
			   (cond ,,@(mapcar (lambda (x)
					      ``((eq ',,(intern (string x) "KEYWORD") ,g!-it)
						 (setf ,g!-it (yield (,,x)))))
					    handlers)
				 (t (return ,g!-it))))))))
     ,@body))


(labels ((noerror-read-byte (stream)
	   ;; For now iterators don't allow to correctly handle exceptions
	   ;; hence this KLUDGE
	   (let ((it (handler-case (read-byte stream)
		       (end-of-file () nil))))
	     (if it
		 (bits<- it)))))
  (defiter byte-reader (stream)
    (let ((bytes-read 0))
      (labels ((read-byte! ()
		 (setf bytes-read (mod (1+ bytes-read) 4))
		 (noerror-read-byte stream))
	       (align-32-bits ()
		 ;; (format t "Aligning to 32 bits, bytes-read: ~a!~%" bytes-read)
		 (iter (while (not (zerop bytes-read)))
		       (read-byte!)))
	       (skip-32-bits ()
		 (iter (for i from 1 to 4)
		       (noerror-read-byte stream))))
	;; TODO : more fast skipping of 32-bit chunks
	(with-yield-dispatch (align-32-bits skip-32-bits)
	  (iter outer (while t)
		(let ((it (read-byte!)))
		  (if (not it)
		      (return-from outer nil)
		      (yield! it)))))))))
      
(defmacro cur-byte ()
  `(if (equal bits-in-byte offset)
       (progn (setf offset 0
		    cur-byte (inext my-byte-reader))
	      cur-byte)
       cur-byte))
	      
(defmacro starting-bits ()
  ;; Ok, maybe this code is not optimal, but it's the first version ...
  `(let ((it (bit-and (cur-byte) (bit- (lshift (bits<- 1) nbits) (bits<- 1)))))
     (setf cur-byte (rshift cur-byte nbits))
     (incf offset nbits)
     it))

(defmacro remaining-bits ()
  `(let ((it (cons (cur-byte) ; the byte should already be zero-padded from the right -- nothing to truncate
		   (- bits-in-byte offset)))) ; with this one we really should remember, how many bits did we read
     (decf nbits (- bits-in-byte offset))
     (setf offset bits-in-byte)
     it))

(defmacro whole-byte ()
  `(let ((it (cur-byte)))
     (decf nbits bits-in-byte)
     (setf offset bits-in-byte)
     it))

(defun assemble-bit-chunks (lst)
  (let ((res (bits<- 0)))
    (iter (for elt in lst)
	  (setf res (if (consp elt)
			(bit+ (lshift res (cdr elt))
			      (car elt))
			(bit+ (lshift res bits-in-byte) elt))))
    res))
	      
(defiter bit-reader (stream)
  "We always return bit-vector with as much bits, as requested -- without 0-padding from the left"
  (let ((my-byte-reader (byte-reader stream))
	(offset bits-in-byte)
	(cur-byte 0))
    (labels ((align-32-bits ()
	       (setf offset bits-in-byte)
	       (inext-or-error my-byte-reader :align-32-bits))
	     (skip-32-bits ()
	       (setf offset bits-in-byte)
	       (inext-or-error my-byte-reader :skip-32-bits)))
      (with-yield-dispatch (align-32-bits skip-32-bits)
	(iter (while t)
	      (let ((nbits (last-yield-value)))
		(if (<= nbits (- bits-in-byte offset))
		    (yield! (bits<- (starting-bits)
				    nbits))
		    (let ((res (list (remaining-bits))))
		      (iter (while (>= nbits bits-in-byte))
			    (push (whole-byte) res))
		      (push (starting-bits) res)
		      (yield! (bits<- (assemble-bit-chunks res)
				      nbits))))))))))

(defparameter *bit-reader* nil)

;; This low-level code is pretty much literal translation from BitstreamReader.cpp

(defun read-vbr (nbits)
  (labels ((rec (acc bit-shift)
	     (let* ((piece (inext-or-error *bit-reader* nbits))
		    (acc (bit-ior acc (lshift (bit-and piece
						       (bits<- (1- (ash 1 (1- nbits)))))
					      bit-shift))))
	       (if (not (zerop (int<- (bit-and piece (lshift (bits<- 1) (1- nbits))))))
		   acc
		   (rec acc (+ bit-shift (1- nbits)))))))
    (int<- (rec #*0 0))))

(defun read-bitcode-header ()
  (let ((it (int<- (inext-or-error *bit-reader* bits-in-byte))))
    (if (not (equal (char-code #\b) it))
	(error 'llvm-bitcode-error "First byte of LLVM bitcode should be #\b, but got: ~a" it)))
  (let ((it (int<- (inext-or-error *bit-reader* bits-in-byte))))
    (if (not (equal (char-code #\c) it))
	(error 'llvm-bitcode-error "Second byte of LLVM bitcode should be #\c, but got: ~a" it))))
  
;; TODO : actually implement also the wrapper format

(defun read-magic-number (&optional expected-number)
  (let ((res (iter (for i from 1 to 4)
		   (collect (inext-or-error *bit-reader* 4)))))
    (when expected-number
      (let ((expected-number (mapcar #'bits<- expected-number)))
	(if (not (equal expected-number res))
	    (error 'llvm-bitcode-error "Expected magic number ~a, but got: ~a" expected-number res))))
    res))
  
(defun decode-char6 (code)
  (let ((it (int<- code)))
    (assert (< it 64))
    (cond ((< it 26) (code-char (+ (char-code #\a) it)))
	  ((< it (+ 26 26)) (code-char (+ (char-code #\A) (- it 26))))
	  ((< it (+ 26 26 10)) (code-char (+ (char-code #\0) (- it 26 26))))
	  ((= it 62) #\.)
	  ((= it 63) #\_)
	  (t (error 'llvm-bitcode-error "Failed to decode ~a as char6" it)))))

(defun encode-char6 (char)
  (bits<- (cond ((char= char #\.) 62)
		  ((char= char #\_) 63)
		  (t (let ((code (char-code char)))
		       (cond ((and (>= code (char-code #\a))
				   (<= code (char-code #\z)))
			      (- code (char-code #\a)))
			     ((and (>= code (char-code #\A))
				   (<= code (char-code #\Z)))
			      (+ (- code (char-code #\A)) 26))
			     ((and (>= code (char-code #\0))
				   (<= code (char-code #\9)))
			      (+ (- code (char-code #\A)) 26 26))
			     (t (error 'llvm-bitcode-error "Don't know how to encode ~a as char6" char))))))
	  6))

(defun bits<- (smth &optional length)
  "Wrapper around BIT-SMASHER's BIT<-, which creates bit-array of fixed width"
  (let ((it (bit-smasher:bits<- smth)))
    (cond ((not length) it)
	  ((>= (length it) length)
	   (progn ;; TODO : check that sequence begins with zeroes
	     (subseq it (- (length it) length))))
	  (t (concatenate 'bit-vector (make-array (- length (length it))
						  :element-type 'bit :initial-element 0)
			  it)))))
      
(defun read-char6 ()
  (decode-char6 (inext-or-error *bit-reader* 6)))

(defun read-fixint (nbits)
  (int<- (inext-or-error *bit-reader* nbits)))

;; TODO : I don't know why it keeps talking 'Deleting unreachable code' here...
(defun read-abbrev-op ()
  (if (bit-eqv #*1 (inext-or-error *bit-reader* 1))
      `(:literal ,(read-vbr 8))
      (let ((op (read-fixint 3)))
	(cond
	  ((equal 1 op) `(:fixed ,(read-vbr 5)))
	  ((equal 2 op) `(:vbr ,(read-vbr 5)))
	  ((equal 3 op) :array)
	  ((equal 4 op) :char6)
	  ((equal 5 op) :blob)
	  (t (error 'llvm-bitcode-error "Don't know how to read this abbrev op:~a" op))))))
	


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-array-reader (size elt)
    (alexandria:with-gensyms (g!-i)
      `(iter (for ,g!-i from 1 to ,(make-atom-reader size))
	     (collect ,(make-atom-reader elt)))))
  (defun make-atom-reader (spec)
    (if (symbolp spec)
	(cond ((eq 'align-32-bits spec) `(inext-or-error *bit-reader* :align-32-bits))
	      ((eq 'abbrev-op spec) `(read-abbrev-op))
	      (t (error "Don't know how to read: ~a" spec)))
	(ecase (car spec)
	  (int `(read-fixint ,(cadr spec)))
	  (vbr `(int<- (read-vbr ,(cadr spec))))))))



(defmacro define-reader (name &body specs)
  ;; TODO : actually implement the thing, probably doubled by definition of the writer
  (let ((cons-specs (remove-if-not #'consp specs)))
    `(defun ,(intern #?"READ-$(name)") ()
       (let ,(mapcar #'car cons-specs)
	 ,@(mapcar (lambda (spec)
		     (if (symbolp spec)
			 (make-atom-reader spec)
			 `(setf ,(car spec) ,(if (and (consp (cadr spec))
						      (eq 'array (caadr spec)))
						 (make-array-reader (cadadr spec) (nth 2 (cadr spec)))
						 (make-atom-reader (cadr spec))))))
		   specs)
	 (list ,@(mapcar (lambda (spec)
			   `(cons ',(car spec) ,(car spec)))
			 cons-specs))))))



;; BTW, very likely the bitcode writer will not be one-pass operation
;; (as I would need to know block sizes before I can actually write them)
;; I wonder what structure would capture this best...

(define-reader block
  (block-id (vbr 8)) (abbrev-len (vbr 4)) align-32-bits (block-len (int 32)))

(define-reader end-block
  align-32-bits)

(define-reader unabbrev-record
  (code (vbr 6))
  (fields (array (vbr 6) (vbr 6))))

(define-reader define-abbrev
  (specs (array (vbr 5) abbrev-op)))

(defconstant end-block-code 0)
(defconstant block-code 1)
(defconstant define-abbrev-code 2)
(defconstant unabbrev-record-code 3)
(defconstant first-application-abbrev-code 4)

(defconstant blockinfo-block-id 0)
(defconstant first-application-block-id 8)

(defconstant blockinfo-code-setbid 1)
(defconstant blockinfo-code-blockname 2)
(defconstant blockinfo-code-setrecordname 3)

(defmacro with-primitive-commands ((&rest commands) &body body)
  "Yes, it deliberately injects CODE into BODY"
  `(let ((code (read-fixint abbrev-id-width)))
     (cond 
       ,@(mapcar (lambda (x)
		   `((equal ,(intern #?"$(x)-CODE") code)
		     `(,,(intern (string x) "KEYWORD")
			 ,(,(intern #?"READ-$(x)")))))
		 commands)
       (t (progn ,@body)))))

(defun get-handler (code)
  (let ((handler (gethash code abbrev-record-handlers)))
    (or handler
	(error 'llvm-bitcode-read-error "Unrecognized abbreviated record code: ~a" code))))

(defun lexer-advance ()
  "On successful invocations returns primitives of the stream.
They are used to modify reader's state on the higher level."
  (with-primitive-commands (end-block block define-abbrev unabbrev-record)
    ;; handle defined abbrevs
    `(:abbrev-record ((code . ,code) ,@(funcall (get-handler code))))))

(defun skip-block (len-in-32-bits)
  (iter (for i from 1 to len-in-32-bits)
	(inext-or-error *bit-reader* :skip-32-bits)))
	
(defun parse-standard-block (form)
  (cond ((equal blockinfo-block-id (cdr (assoc 'block-id form)))
	 (progn (let ((abbrev-id-width (cdr (assoc 'abbrev-len form)))
		      cur-block
		      ;; probably should set some more things here
		      )
		  (iter (while t)
			(destructuring-bind (type form) (lexer-advance)
			  (ecase type
			    (:end-block (terminate))
			    (:block (warn "subblock encountered inside BLOCKINFO block -- skipping")
			      (skip-block (cdr (assoc 'block-len form))))
			    (:define-abbrev (setf-record-handler cur-block (cdr (assoc 'specs form))))
			    ((:unabbrev-record :abbrev-record)
			     (ecase (cdr (assoc 'code form))
			       (blockinfo-code-setbid (setf cur-block (get-block-struct (car (cdr (assoc 'fields form))))))
			       (blockinfo-code-blockname (setf (name cur-block) (car (cdr (assoc 'fields form)))))
			       (blockinfo-code-setrecordname (setf-record-name cur-block
									       (car (cdr (assoc 'fields form)))
									       (cadr (cdr (assoc 'fields form)))))))
			    ))))
		;; Because upper layer can't really fill BLOCKINFO blocks, we recurse just here
		(parser-advance)))
	(t (error 'llvm-bitcode-error "Don't know the standard block with code: ~a" (cdr (assoc 'block-id form))))))

(defun parser-advance (&optional (recursive nil))
  ;; Let's first assume that RECURSIVE is T -- we have some env around us
  ;; Furthermore, we are, maybe, in some subblock.
  (destructuring-bind (type form) (lexer-advance)
    (ecase type
      (:end-block ;; handle on the level above
       `(:end-block nil))
      (:block ;; set up env, recursively read all the records in the block
	  (if (> first-application-block-id (cdr (assoc 'block-id form)))
	      (parse-standard-block form)
	      (parse-application-block form)))
      (:define-abbrev ;; this has different meaning, depending on whether it's inside
       ...)
      ;; For above layers it's not important, was record abbreviated or not
      (:unabbrev-record `(:record ,form))
      (:abbrev-record `(:record ,form))))))


