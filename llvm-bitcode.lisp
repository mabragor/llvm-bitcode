;;;; llvm-bitcode.lisp

(in-package #:llvm-bitcode)

(quasiquote-2.0:enable-quasiquote-2.0)
(cl-interpol:enable-interpol-syntax)

;;; "llvm-bitcode" goes here. Hacks and glory await!

(defconstant bits-in-byte 8)

(defparameter abbrev-id-width 2)

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
  `(progn (format t "  About to read starting bits~%")
	  (format t "Nbits: ~a" nbits)
	  (if (zerop nbits) ; if we actually read nothing, don't load new byte
	      #*
	      (let ((it (bits<- (cur-byte) nbits)))
		(setf cur-byte (rshift cur-byte nbits))
		(incf offset nbits)
		it))))

(defmacro remaining-bits ()
  `(progn (format t "  About to read remaining bits~%")
	  (if (zerop nbits) ; if we actually read nothing, don't load new byte
	      #*
	      (let ((it (bits<- (cur-byte) (- bits-in-byte offset))))
		(decf nbits (- bits-in-byte offset))
		(setf offset bits-in-byte)
		it))))

(defmacro whole-byte ()
  `(progn (format t "  About to read whole byte~%")
	  (let ((it (cur-byte)))
	    (decf nbits bits-in-byte)
	    (setf offset bits-in-byte)
	    it)))

(defun assemble-bit-chunks (lst)
  (format t "  Assembling bit chunks: ~a~%" lst)
  (apply #'concatenate (cons 'bit-vector
			     lst)))
	      
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
		(format t "Nbits1: ~a offset: ~a~%" nbits offset)
		(if (<= nbits (- bits-in-byte (mod offset bits-in-byte)))
		    (yield! (bits<- (starting-bits)
				    nbits))
		    (let ((res (list (remaining-bits))))
		      (iter (while (>= nbits bits-in-byte))
			    (push (whole-byte) res))
		      (push (starting-bits) res)
		      (yield! (assemble-bit-chunks res))))))))))

(defparameter *bit-reader* nil)

;; This low-level code was pretty much literal translation from BitstreamReader.cpp
;; But now it's way more lispy and is like 2 times shorter =) I dunno about the speed though...

(defun read-vbr (nbits)
  (let (res)
    (iter (for piece next (inext-or-error *bit-reader* nbits))
	  (push (bits<- piece (1- nbits)) res)
	  (if (equal #*0 (subseq piece 0 1))
	      (terminate)))
    (assemble-bit-chunks res)))
      

(defun read-bitcode-header ()
  (let ((it (int<- (inext-or-error *bit-reader* bits-in-byte))))
    (if (not (equal (char-code #\B) it))
  	(error 'llvm-bitcode-error "First byte of LLVM bitcode should be #\b, but got: ~a" it)))
  (let ((it (int<- (inext-or-error *bit-reader* bits-in-byte))))
    (if (not (equal (char-code #\C) it))
  	(error 'llvm-bitcode-error "Second byte of LLVM bitcode should be #\c, but got: ~a" it)))
  :success)
  
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

(defun lexer-advance ()
  "On successful invocations returns primitives of the stream.
They are used to modify reader's state on the higher level."
  (with-primitive-commands (end-block block define-abbrev unabbrev-record)
    ;; handle defined abbrevs
    `(:abbrev-record ((code . ,code) ,@(funcall (get-handler code))))))

(defun skip-block (len-in-32-bits)
  (iter (for i from 1 to len-in-32-bits)
	(inext-or-error *bit-reader* :skip-32-bits)))

(defclass block-env ()
  ((id :initform (error "Block ID is mandatory.") :initarg :id)
   (name :initarg :name :accessor name)
   (next-abbrev-code :initform first-application-abbrev-code)
   (record-handlers :initform (make-hash-table :test #'equal))
   (record-names :initform (make-hash-table :test #'equal))))


(defun read-array (elt-reader)
  ;; TODO : actually assemble all the numerical constants into one place -- in case of future change
  (let ((len (read-vbr 6)))
    (list :array
	  (iter (for i from 1 to len)
		(collect (funcall elt-reader))))))

(defun read-blob ()
  (let ((len (read-vbr 6)))
    (inext-or-error *bit-reader* :align-32-bits)
    (let ((res (iter (for i from 1 to len)
		     (read-fixint 8))))
      (inext-or-error *bit-reader* :align-32-bits)
      (list :blob res))))
    

(defun mk-reader-thunk (spec-iter)
  (let ((spec (inext-or-error spec-iter)))
    (if (symbolp spec)
	(cond ((eq :array spec) (let ((elt-reader (handler-case (mk-reader-thunk spec-iter)
						    (stop-iteration () (error 'llvm-bitcode-read-error "Spec finished before array was complete")))))
				  (lambda ()
				    (read-array elt-reader))))
	      ((eq :blob spec) #'read-blob)
	      ((eq :char6 spec) #'read-char6)
	      (t (error "Unknown atomic abbrev spec: ~a" spec)))
	(destructuring-bind (type arg) spec
	  (ecase type
	    (:fixed (lambda ()
		      (read-fixint arg)))
	    (:vbr (lambda ()
		    (read-vbr arg)))
	    (:literal (lambda ()
			arg)))))))
    

(defun compile-handler-from-spec (handler-spec)
  (let ((spec-iter (mk-iter handler-spec)))
    (compile nil
	     `(lambda ()
		(list ,@(iter (while t)
			      (collect `(funcall ,(handler-case (mk-reader-thunk spec-iter)
								(stop-iteration () (terminate)))))))))))

(defun setf-record-handler (block handler-spec)
  (with-slots (next-abbrev-code record-handlers) block
    (let ((it (compile-handler-from-spec handler-spec)))
      (setf (gethash next-abbrev-code record-handlers) it)
      (incf next-abbrev-code)
      it)))

(defun setf-record-name (block id str)
  (with-slots (record-names) block
    (setf (gethash id record-names) str)))

(defparameter block-envs (make-hash-table :test #'equal))

(defun get-block-env (id)
  (or (gethash id block-envs)
      (setf (gethash id block-envs)
	    (make-instance 'block-env :id id))))

(defparameter block-env nil)
(defparameter tmp-env nil)

(defun get-handler (code)
  (or (gethash code (slot-value tmp-env 'record-handlers))
      (gethash code (slot-value block-env 'record-handlers))
      (error 'llvm-bitcode-read-error "Unrecognized abbreviated record code: ~a" code)))

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
			     ;; TODO : what if CUR-BLOCK is still NULL?
			     (let ((it (cdr (assoc 'code form))))
			       (cond ((equal blockinfo-code-setbid it)
				      (setf cur-block (get-block-env (car (cdr (assoc 'fields form))))))
				     ((equal blockinfo-code-blockname it)
				      (setf (name cur-block) (car (cdr (assoc 'fields form)))))
				     ((equal blockinfo-code-setrecordname it)
				      (setf-record-name cur-block
							(car (cdr (assoc 'fields form)))
							(cadr (cdr (assoc 'fields form)))))
				     (t (error 'llvm-bitcode-read-error "Unexpected field in BLOCK INFO block: ~a" it)))))
			    ))))
		nil))
	(t (error 'llvm-bitcode-error "Don't know the standard block with code: ~a" (cdr (assoc 'block-id form))))))


(defun make-tmp-env (block-env)
  (with-slots (record-handlers next-abbrev-code id) block-env
    (let ((res (make-instance 'block-env :id id)))
      (setf (slot-value res 'next-abbrev-code) next-abbrev-code)
      res)))
	  
(defun parse-application-block (form)
  (let* ((abbrev-id-width (cdr (assoc 'abbrev-len form)))
	 (block-env (get-block-env (cdr (assoc 'block-id form))))
	 (tmp-env (make-tmp-env block-env)))
    (remove-if-not #'identity
		   (iter (while t)
			 (collect (destructuring-bind (type form)
				      ;; This HANDLER-CASE does automatic closing of blocks at the end of the stream
				      (handler-case (lexer-advance)
					(stop-iteration () (terminate)))
				    (ecase type
				      (:end-block (terminate))
				      (:block (if (> first-application-block-id (cdr (assoc 'block-id form)))
						  (parse-standard-block form)
						  (parse-application-block form)))
				      (:define-abbrev (setf-record-handler tmp-env (cdr (assoc 'specs form))))
				      ((:unabbrev-record :abbrev-record)
				       `(:record ,form))
				      )))))))

(defun parser-advance ()
  "This is just the toplevel wrapper around parse block"
  (destructuring-bind (type form) (lexer-advance)
    (if (eq :block type)
	(if (> first-application-block-id (cdr (assoc 'block-id form)))
	    (parse-standard-block form)
	    (parse-application-block form))
	(error 'llvm-bitcode-read-error "~a encountered on the top level" type))))



(defun read-bc-file (fname)
  (with-open-file (stream fname :element-type `(unsigned-byte ,bits-in-byte))
    (let ((*bit-reader* (bit-reader stream)))
      (read-bitcode-header)
      (read-magic-number)
      (iter (while t)
	    (collect (handler-case (parser-advance)
		       (stop-iteration () (terminate))))))))
