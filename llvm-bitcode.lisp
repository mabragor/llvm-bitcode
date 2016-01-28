;;;; llvm-bitcode.lisp

(in-package #:llvm-bitcode)

;;; "llvm-bitcode" goes here. Hacks and glory await!

(defconstant bits-in-byte 8)

(defiter byte-reader (stream)
  (iter outer (while t)
	(let ((it (handler-case (bits<- (read-byte stream))
		    (end-of-file () (return-from outer nil)))))
	  (yield it))))

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
    (iter (while t)
	  (let ((nbits (last-yield-value)))
	    (if (<= nbits (- bits-in-byte offset))
		(yield (starting-bits))
		(let ((res (list (remaining-bits))))
		  (iter (while (>= nbits bits-in-byte))
			(push (whole-byte) res))
		  (push (starting-bits) res)
		  (yield (assemble-bit-chunks res))))))))
			 
;; (defun read-vbr (nbits)
;;   (labels ((rec ...))
;;     (rec 0

(defun foo (fname)
  (with-open-file (stream fname :element-type `(unsigned-byte ,bits-in-byte))
    (iter (for byte in-it (byte-reader stream))
	  (for i from 1 to 10)
	  (collect byte))))
