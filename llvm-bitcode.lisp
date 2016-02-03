;;;; llvm-bitcode.lisp

(in-package #:llvm-bitcode)

(quasiquote-2.0:enable-quasiquote-2.0)

;;; "llvm-bitcode" goes here. Hacks and glory await!

(defconstant bits-in-byte 8)

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

(defiter byte-reader (stream)
  (let ((bytes-read 0))
    (labels ((read-byte! ()
	       (setf bytes-read (mod (1+ bytes-read) 4))
	       (read-byte stream))
	     (align-32-bits ()
	       (format t "Aligning to 32 bits, bytes-read: ~a!~%" bytes-read)
	       (iter (while (not (zerop bytes-read)))
		     (read-byte!))))
      (with-yield-dispatch (align-32-bits)
	(iter outer (while t)
	      (let ((it (handler-case (bits<- (read-byte!))
			  (end-of-file () (return-from outer nil)))))
		(yield! it)))))))
      
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
  (let ((my-byte-reader (byte-reader stream))
	(offset bits-in-byte)
	(cur-byte 0))
    (labels ((align-32-bits ()
	       (setf offset bits-in-byte)
	       (inext-or-error my-byte-reader :align-32-bits)))
      (with-yield-dispatch (align-32-bits)
	(iter (while t)
	      (let ((nbits (last-yield-value)))
		(if (<= nbits (- bits-in-byte offset))
		    (yield! (starting-bits))
		    (let ((res (list (remaining-bits))))
		      (iter (while (>= nbits bits-in-byte))
			    (push (whole-byte) res))
		      (push (starting-bits) res)
		      (yield! (assemble-bit-chunks res))))))))))

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
    (rec #*0 0)))

(defun foo (fname)
  (with-open-file (stream fname :element-type `(unsigned-byte ,bits-in-byte))
    (iter (for byte in-it (byte-reader stream))
	  (for i from 1 to 10)
	  (collect (int<- byte)))))

(defun foo1 (fname)
  (with-open-file (stream fname :element-type `(unsigned-byte ,bits-in-byte))
    (let ((my-it (byte-reader stream)))
      (list (int<- (inext-or-error my-it))
	    (inext-or-error my-it :align-32-bits)
	    (int<- (inext-or-error my-it))
	    (int<- (inext-or-error my-it))
	    (int<- (inext-or-error my-it))
	    (int<- (inext-or-error my-it))))))
